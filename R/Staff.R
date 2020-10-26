
	#' List Staff
	#'
	#' This function returns a dataframe or json object of Staff
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Staff. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Staff.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Staff') to get more field paths.
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
	#' @concept Staff
	#' @return A list of Staff
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStaff <- function(searchConditionsList = NULL, StaffMNID = F, FileFolderNumber = F, StaffID = F, NameID = F, FullNameLFM = F, FullNameFL = F, FullNameFML = F, StaffWebsite = F, DistrictID = F, StaffNumber = F, IsCurrentStaffEntityYear = F, StaffMeetCount = F, AssignmentCount = F, NonGradedAssignmentCountForTerm = F, NonGradedAssignmentCountNoStudentAssignmentsYTD = F, PercentageOfAssignmentsScoredYTD = F, StudentAssignmentDataString = F, AssignmentDataString = F, ActiveStudentCount = F, DueDateOfLastAssignmentScored = F, DateTimeofLastScoredAssignment = F, NonGradedAssignmentCountNoStudentAssignmentsForTerm = F, GradebookLastAccessedTime = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, GradeLevelLowerBound = F, GradeLevelUpperBound = F, IsActiveForDistrict = F, FamilyStudentAccessStaffNameToUse = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Staff", objectName = "Staff", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Staff
	#'
	#' This function returns a dataframe or json object of a Staff
	#' @param StaffID The ID of the Staff to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Staff. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Staff.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Staff') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return A dataframe or of Staff
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStaff <- function(StaffID, StaffMNID = F, FileFolderNumber = F, NameID = F, FullNameLFM = F, FullNameFL = F, FullNameFML = F, StaffWebsite = F, DistrictID = F, StaffNumber = F, IsCurrentStaffEntityYear = F, StaffMeetCount = F, AssignmentCount = F, NonGradedAssignmentCountForTerm = F, NonGradedAssignmentCountNoStudentAssignmentsYTD = F, PercentageOfAssignmentsScoredYTD = F, StudentAssignmentDataString = F, AssignmentDataString = F, ActiveStudentCount = F, DueDateOfLastAssignmentScored = F, DateTimeofLastScoredAssignment = F, NonGradedAssignmentCountNoStudentAssignmentsForTerm = F, GradebookLastAccessedTime = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, GradeLevelLowerBound = F, GradeLevelUpperBound = F, IsActiveForDistrict = F, FamilyStudentAccessStaffNameToUse = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StaffID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Staff", objectName = "Staff", objectId = StaffID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Staff
	#'
	#' This function deletes a Staff
	#' @param StaffID The ID of the Staff to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return The StaffID of the deleted Staff.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStaff <- function(StaffID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Staff", objectName = "Staff", objectId = StaffID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Staff
	#'
	#' This function creates a Staff
	#' @param fieldNames The field values to give the created Staff. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return A newly created Staff
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStaff <- function(FileFolderNumber = NULL, NameID = NULL, StaffWebsite = NULL, DistrictID = NULL, StaffNumber = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Staff", objectName = "Staff", body = list(DataObject = body), searchFields = append("StaffID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Staff
	#'
	#' This function modifies a Staff
	#' @param fieldNames The field values to give the modified Staff. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return The modified Staff
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStaff <- function(StaffID, FileFolderNumber = NULL, NameID = NULL, StaffWebsite = NULL, DistrictID = NULL, StaffNumber = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Staff", objectName = "Staff", objectId = StaffID, body = list(DataObject = body), searchFields = append("StaffID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StaffConfigSystems
	#'
	#' This function returns a dataframe or json object of StaffConfigSystems
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StaffConfigSystems. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StaffConfigSystems.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StaffConfigSystem') to get more field paths.
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
	#' @concept Staff
	#' @return A list of StaffConfigSystems
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStaffConfigSystems <- function(searchConditionsList = NULL, ConfigSystemID = F, StaffNumberMask = F, StaffNumberStartValue = F, AutoGenerateSecurityUser = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, NewTeacherAccessUserMessageSubject = F, NewTeacherAccessUserMessageContent = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Staff", objectName = "ConfigSystem", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StaffConfigSystem
	#'
	#' This function returns a dataframe or json object of a StaffConfigSystem
	#' @param StaffConfigSystemID The ID of the StaffConfigSystem to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StaffConfigSystem. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StaffConfigSystem.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StaffConfigSystem') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return A dataframe or of StaffConfigSystem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStaffConfigSystem <- function(StaffConfigSystemID, ConfigSystemID = F, StaffNumberMask = F, StaffNumberStartValue = F, AutoGenerateSecurityUser = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, NewTeacherAccessUserMessageSubject = F, NewTeacherAccessUserMessageContent = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StaffConfigSystemID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Staff", objectName = "ConfigSystem", objectId = StaffConfigSystemID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StaffConfigSystem
	#'
	#' This function deletes a StaffConfigSystem
	#' @param StaffConfigSystemID The ID of the StaffConfigSystem to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return The StaffConfigSystemID of the deleted StaffConfigSystem.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStaffConfigSystem <- function(StaffConfigSystemID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Staff", objectName = "ConfigSystem", objectId = StaffConfigSystemID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StaffConfigSystem
	#'
	#' This function creates a StaffConfigSystem
	#' @param fieldNames The field values to give the created StaffConfigSystem. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return A newly created StaffConfigSystem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStaffConfigSystem <- function(StaffNumberMask = NULL, StaffNumberStartValue = NULL, AutoGenerateSecurityUser = NULL, NewTeacherAccessUserMessageSubject = NULL, NewTeacherAccessUserMessageContent = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Staff", objectName = "ConfigSystem", body = list(DataObject = body), searchFields = append("ConfigSystemID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StaffConfigSystem
	#'
	#' This function modifies a StaffConfigSystem
	#' @param fieldNames The field values to give the modified StaffConfigSystem. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return The modified StaffConfigSystem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStaffConfigSystem <- function(ConfigSystemID, StaffNumberMask = NULL, StaffNumberStartValue = NULL, AutoGenerateSecurityUser = NULL, NewTeacherAccessUserMessageSubject = NULL, NewTeacherAccessUserMessageContent = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Staff", objectName = "ConfigSystem", objectId = ConfigSystemID, body = list(DataObject = body), searchFields = append("ConfigSystemID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List NextStaffNumbers
	#'
	#' This function returns a dataframe or json object of NextStaffNumbers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NextStaffNumbers. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NextStaffNumbers.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NextStaffNumber') to get more field paths.
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
	#' @concept Staff
	#' @return A list of NextStaffNumbers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNextStaffNumbers <- function(searchConditionsList = NULL, NextStaffNumberID = F, MaskPrefix = F, SequenceNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Staff", objectName = "NextStaffNumber", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NextStaffNumber
	#'
	#' This function returns a dataframe or json object of a NextStaffNumber
	#' @param NextStaffNumberID The ID of the NextStaffNumber to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NextStaffNumber. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NextStaffNumber.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NextStaffNumber') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return A dataframe or of NextStaffNumber
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNextStaffNumber <- function(NextStaffNumberID, MaskPrefix = F, SequenceNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NextStaffNumberID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Staff", objectName = "NextStaffNumber", objectId = NextStaffNumberID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NextStaffNumber
	#'
	#' This function deletes a NextStaffNumber
	#' @param NextStaffNumberID The ID of the NextStaffNumber to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return The NextStaffNumberID of the deleted NextStaffNumber.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNextStaffNumber <- function(NextStaffNumberID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Staff", objectName = "NextStaffNumber", objectId = NextStaffNumberID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NextStaffNumber
	#'
	#' This function creates a NextStaffNumber
	#' @param fieldNames The field values to give the created NextStaffNumber. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return A newly created NextStaffNumber
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNextStaffNumber <- function(MaskPrefix = NULL, SequenceNumber = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Staff", objectName = "NextStaffNumber", body = list(DataObject = body), searchFields = append("NextStaffNumberID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NextStaffNumber
	#'
	#' This function modifies a NextStaffNumber
	#' @param fieldNames The field values to give the modified NextStaffNumber. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return The modified NextStaffNumber
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNextStaffNumber <- function(NextStaffNumberID, MaskPrefix = NULL, SequenceNumber = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Staff", objectName = "NextStaffNumber", objectId = NextStaffNumberID, body = list(DataObject = body), searchFields = append("NextStaffNumberID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StaffTypes
	#'
	#' This function returns a dataframe or json object of StaffTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StaffTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StaffTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StaffType') to get more field paths.
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
	#' @concept Staff
	#' @return A list of StaffTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStaffTypes <- function(searchConditionsList = NULL, StaffTypeID = F, DistrictID = F, Code = F, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CodeDescription = F, SchoolYearID = F, StaffTypeIDClonedFrom = F, StaffTypeIDClonedTo = F, EdFiStaffClassificationDescriptorID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Staff", objectName = "StaffType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StaffType
	#'
	#' This function returns a dataframe or json object of a StaffType
	#' @param StaffTypeID The ID of the StaffType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StaffType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StaffType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StaffType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return A dataframe or of StaffType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStaffType <- function(StaffTypeID, DistrictID = F, Code = F, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CodeDescription = F, SchoolYearID = F, StaffTypeIDClonedFrom = F, StaffTypeIDClonedTo = F, EdFiStaffClassificationDescriptorID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StaffTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Staff", objectName = "StaffType", objectId = StaffTypeID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StaffType
	#'
	#' This function deletes a StaffType
	#' @param StaffTypeID The ID of the StaffType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return The StaffTypeID of the deleted StaffType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStaffType <- function(StaffTypeID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Staff", objectName = "StaffType", objectId = StaffTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StaffType
	#'
	#' This function creates a StaffType
	#' @param fieldNames The field values to give the created StaffType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return A newly created StaffType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStaffType <- function(DistrictID = NULL, Code = NULL, Description = NULL, SchoolYearID = NULL, StaffTypeIDClonedFrom = NULL, EdFiStaffClassificationDescriptorID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Staff", objectName = "StaffType", body = list(DataObject = body), searchFields = append("StaffTypeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StaffType
	#'
	#' This function modifies a StaffType
	#' @param fieldNames The field values to give the modified StaffType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return The modified StaffType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStaffType <- function(StaffTypeID, DistrictID = NULL, Code = NULL, Description = NULL, SchoolYearID = NULL, StaffTypeIDClonedFrom = NULL, EdFiStaffClassificationDescriptorID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Staff", objectName = "StaffType", objectId = StaffTypeID, body = list(DataObject = body), searchFields = append("StaffTypeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StaffStaffTypes
	#'
	#' This function returns a dataframe or json object of StaffStaffTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StaffStaffTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StaffStaffTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StaffStaffType') to get more field paths.
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
	#' @concept Staff
	#' @return A list of StaffStaffTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStaffStaffTypes <- function(searchConditionsList = NULL, StaffStaffTypeID = F, StaffTypeID = F, StaffID = F, IsPrimary = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartDate = F, EndDate = F, StaffStaffTypeIDClonedFrom = F, PositionDescription = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Staff", objectName = "StaffStaffType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StaffStaffType
	#'
	#' This function returns a dataframe or json object of a StaffStaffType
	#' @param StaffStaffTypeID The ID of the StaffStaffType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StaffStaffType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StaffStaffType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StaffStaffType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return A dataframe or of StaffStaffType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStaffStaffType <- function(StaffStaffTypeID, StaffTypeID = F, StaffID = F, IsPrimary = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartDate = F, EndDate = F, StaffStaffTypeIDClonedFrom = F, PositionDescription = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StaffStaffTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Staff", objectName = "StaffStaffType", objectId = StaffStaffTypeID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StaffStaffType
	#'
	#' This function deletes a StaffStaffType
	#' @param StaffStaffTypeID The ID of the StaffStaffType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return The StaffStaffTypeID of the deleted StaffStaffType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStaffStaffType <- function(StaffStaffTypeID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Staff", objectName = "StaffStaffType", objectId = StaffStaffTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StaffStaffType
	#'
	#' This function creates a StaffStaffType
	#' @param fieldNames The field values to give the created StaffStaffType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return A newly created StaffStaffType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStaffStaffType <- function(StaffTypeID = NULL, StaffID = NULL, IsPrimary = NULL, StartDate = NULL, EndDate = NULL, StaffStaffTypeIDClonedFrom = NULL, PositionDescription = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Staff", objectName = "StaffStaffType", body = list(DataObject = body), searchFields = append("StaffStaffTypeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StaffStaffType
	#'
	#' This function modifies a StaffStaffType
	#' @param fieldNames The field values to give the modified StaffStaffType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return The modified StaffStaffType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStaffStaffType <- function(StaffStaffTypeID, StaffTypeID = NULL, StaffID = NULL, IsPrimary = NULL, StartDate = NULL, EndDate = NULL, StaffStaffTypeIDClonedFrom = NULL, PositionDescription = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Staff", objectName = "StaffStaffType", objectId = StaffStaffTypeID, body = list(DataObject = body), searchFields = append("StaffStaffTypeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StaffEntityYears
	#'
	#' This function returns a dataframe or json object of StaffEntityYears
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StaffEntityYears. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StaffEntityYears.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StaffEntityYear') to get more field paths.
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
	#' @concept Staff
	#' @return A list of StaffEntityYears
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStaffEntityYears <- function(searchConditionsList = NULL, StaffEntityYearID = F, StaffID = F, EntityID = F, IsActive = F, IsDisciplineOfficer = F, IsTeacher = F, TeacherNumber = F, IsSubstituteTeacher = F, RoomIDDefault = F, SchoolYearID = F, IsCareerCenterCounselor = F, StaffDepartmentSummary = F, StaffEntityYearIDClonedFrom = F, StaffEntityYearClonedTo = F, IsAdditionalStaffOnNotification = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Staff", objectName = "StaffEntityYear", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StaffEntityYear
	#'
	#' This function returns a dataframe or json object of a StaffEntityYear
	#' @param StaffEntityYearID The ID of the StaffEntityYear to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StaffEntityYear. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StaffEntityYear.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StaffEntityYear') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return A dataframe or of StaffEntityYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStaffEntityYear <- function(StaffEntityYearID, StaffID = F, EntityID = F, IsActive = F, IsDisciplineOfficer = F, IsTeacher = F, TeacherNumber = F, IsSubstituteTeacher = F, RoomIDDefault = F, SchoolYearID = F, IsCareerCenterCounselor = F, StaffDepartmentSummary = F, StaffEntityYearIDClonedFrom = F, StaffEntityYearClonedTo = F, IsAdditionalStaffOnNotification = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StaffEntityYearID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Staff", objectName = "StaffEntityYear", objectId = StaffEntityYearID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StaffEntityYear
	#'
	#' This function deletes a StaffEntityYear
	#' @param StaffEntityYearID The ID of the StaffEntityYear to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return The StaffEntityYearID of the deleted StaffEntityYear.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStaffEntityYear <- function(StaffEntityYearID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Staff", objectName = "StaffEntityYear", objectId = StaffEntityYearID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StaffEntityYear
	#'
	#' This function creates a StaffEntityYear
	#' @param fieldNames The field values to give the created StaffEntityYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return A newly created StaffEntityYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStaffEntityYear <- function(StaffID = NULL, EntityID = NULL, IsActive = NULL, IsDisciplineOfficer = NULL, IsTeacher = NULL, TeacherNumber = NULL, IsSubstituteTeacher = NULL, RoomIDDefault = NULL, SchoolYearID = NULL, IsCareerCenterCounselor = NULL, StaffEntityYearIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Staff", objectName = "StaffEntityYear", body = list(DataObject = body), searchFields = append("StaffEntityYearID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StaffEntityYear
	#'
	#' This function modifies a StaffEntityYear
	#' @param fieldNames The field values to give the modified StaffEntityYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return The modified StaffEntityYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStaffEntityYear <- function(StaffEntityYearID, StaffID = NULL, EntityID = NULL, IsActive = NULL, IsDisciplineOfficer = NULL, IsTeacher = NULL, TeacherNumber = NULL, IsSubstituteTeacher = NULL, RoomIDDefault = NULL, SchoolYearID = NULL, IsCareerCenterCounselor = NULL, StaffEntityYearIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Staff", objectName = "StaffEntityYear", objectId = StaffEntityYearID, body = list(DataObject = body), searchFields = append("StaffEntityYearID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StaffDepartments
	#'
	#' This function returns a dataframe or json object of StaffDepartments
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StaffDepartments. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StaffDepartments.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StaffDepartment') to get more field paths.
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
	#' @concept Staff
	#' @return A list of StaffDepartments
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStaffDepartments <- function(searchConditionsList = NULL, StaffDepartmentID = F, StaffID = F, DepartmentID = F, IsDefaultDepartment = F, StaffDepartmentIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Staff", objectName = "StaffDepartment", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StaffDepartment
	#'
	#' This function returns a dataframe or json object of a StaffDepartment
	#' @param StaffDepartmentID The ID of the StaffDepartment to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StaffDepartment. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StaffDepartment.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StaffDepartment') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return A dataframe or of StaffDepartment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStaffDepartment <- function(StaffDepartmentID, StaffID = F, DepartmentID = F, IsDefaultDepartment = F, StaffDepartmentIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StaffDepartmentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Staff", objectName = "StaffDepartment", objectId = StaffDepartmentID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StaffDepartment
	#'
	#' This function deletes a StaffDepartment
	#' @param StaffDepartmentID The ID of the StaffDepartment to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return The StaffDepartmentID of the deleted StaffDepartment.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStaffDepartment <- function(StaffDepartmentID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Staff", objectName = "StaffDepartment", objectId = StaffDepartmentID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StaffDepartment
	#'
	#' This function creates a StaffDepartment
	#' @param fieldNames The field values to give the created StaffDepartment. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return A newly created StaffDepartment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStaffDepartment <- function(StaffID = NULL, DepartmentID = NULL, IsDefaultDepartment = NULL, StaffDepartmentIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Staff", objectName = "StaffDepartment", body = list(DataObject = body), searchFields = append("StaffDepartmentID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StaffDepartment
	#'
	#' This function modifies a StaffDepartment
	#' @param fieldNames The field values to give the modified StaffDepartment. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return The modified StaffDepartment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStaffDepartment <- function(StaffDepartmentID, StaffID = NULL, DepartmentID = NULL, IsDefaultDepartment = NULL, StaffDepartmentIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Staff", objectName = "StaffDepartment", objectId = StaffDepartmentID, body = list(DataObject = body), searchFields = append("StaffDepartmentID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Departments
	#'
	#' This function returns a dataframe or json object of Departments
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Departments. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Departments.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Department') to get more field paths.
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
	#' @concept Staff
	#' @return A list of Departments
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDepartments <- function(searchConditionsList = NULL, DepartmentID = F, SchoolYearID = F, EntityID = F, EntityGroupKey = F, Code = F, Description = F, StaffIDDepartmentHead = F, DepartmentIDClonedFrom = F, CodeDescription = F, DepartmentIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Staff", objectName = "Department", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Department
	#'
	#' This function returns a dataframe or json object of a Department
	#' @param DepartmentID The ID of the Department to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Department. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Department.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Department') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return A dataframe or of Department
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDepartment <- function(DepartmentID, SchoolYearID = F, EntityID = F, EntityGroupKey = F, Code = F, Description = F, StaffIDDepartmentHead = F, DepartmentIDClonedFrom = F, CodeDescription = F, DepartmentIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DepartmentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Staff", objectName = "Department", objectId = DepartmentID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Department
	#'
	#' This function deletes a Department
	#' @param DepartmentID The ID of the Department to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return The DepartmentID of the deleted Department.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDepartment <- function(DepartmentID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Staff", objectName = "Department", objectId = DepartmentID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Department
	#'
	#' This function creates a Department
	#' @param fieldNames The field values to give the created Department. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return A newly created Department
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDepartment <- function(SchoolYearID = NULL, EntityID = NULL, EntityGroupKey = NULL, Code = NULL, Description = NULL, StaffIDDepartmentHead = NULL, DepartmentIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Staff", objectName = "Department", body = list(DataObject = body), searchFields = append("DepartmentID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Department
	#'
	#' This function modifies a Department
	#' @param fieldNames The field values to give the modified Department. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return The modified Department
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDepartment <- function(DepartmentID, SchoolYearID = NULL, EntityID = NULL, EntityGroupKey = NULL, Code = NULL, Description = NULL, StaffIDDepartmentHead = NULL, DepartmentIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Staff", objectName = "Department", objectId = DepartmentID, body = list(DataObject = body), searchFields = append("DepartmentID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ScheduleBlockerDisplayPeriods
	#'
	#' This function returns a dataframe or json object of ScheduleBlockerDisplayPeriods
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ScheduleBlockerDisplayPeriods. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ScheduleBlockerDisplayPeriods.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ScheduleBlockerDisplayPeriod') to get more field paths.
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
	#' @concept Staff
	#' @return A list of ScheduleBlockerDisplayPeriods
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listScheduleBlockerDisplayPeriods <- function(searchConditionsList = NULL, ScheduleBlockerDisplayPeriodID = F, ScheduleBlockerID = F, DisplayPeriodID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Staff", objectName = "ScheduleBlockerDisplayPeriod", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ScheduleBlockerDisplayPeriod
	#'
	#' This function returns a dataframe or json object of a ScheduleBlockerDisplayPeriod
	#' @param ScheduleBlockerDisplayPeriodID The ID of the ScheduleBlockerDisplayPeriod to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ScheduleBlockerDisplayPeriod. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ScheduleBlockerDisplayPeriod.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ScheduleBlockerDisplayPeriod') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return A dataframe or of ScheduleBlockerDisplayPeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getScheduleBlockerDisplayPeriod <- function(ScheduleBlockerDisplayPeriodID, ScheduleBlockerID = F, DisplayPeriodID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ScheduleBlockerDisplayPeriodID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Staff", objectName = "ScheduleBlockerDisplayPeriod", objectId = ScheduleBlockerDisplayPeriodID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ScheduleBlockerDisplayPeriod
	#'
	#' This function deletes a ScheduleBlockerDisplayPeriod
	#' @param ScheduleBlockerDisplayPeriodID The ID of the ScheduleBlockerDisplayPeriod to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return The ScheduleBlockerDisplayPeriodID of the deleted ScheduleBlockerDisplayPeriod.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteScheduleBlockerDisplayPeriod <- function(ScheduleBlockerDisplayPeriodID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Staff", objectName = "ScheduleBlockerDisplayPeriod", objectId = ScheduleBlockerDisplayPeriodID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ScheduleBlockerDisplayPeriod
	#'
	#' This function creates a ScheduleBlockerDisplayPeriod
	#' @param fieldNames The field values to give the created ScheduleBlockerDisplayPeriod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return A newly created ScheduleBlockerDisplayPeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createScheduleBlockerDisplayPeriod <- function(ScheduleBlockerID = NULL, DisplayPeriodID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Staff", objectName = "ScheduleBlockerDisplayPeriod", body = list(DataObject = body), searchFields = append("ScheduleBlockerDisplayPeriodID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ScheduleBlockerDisplayPeriod
	#'
	#' This function modifies a ScheduleBlockerDisplayPeriod
	#' @param fieldNames The field values to give the modified ScheduleBlockerDisplayPeriod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return The modified ScheduleBlockerDisplayPeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyScheduleBlockerDisplayPeriod <- function(ScheduleBlockerDisplayPeriodID, ScheduleBlockerID = NULL, DisplayPeriodID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Staff", objectName = "ScheduleBlockerDisplayPeriod", objectId = ScheduleBlockerDisplayPeriodID, body = list(DataObject = body), searchFields = append("ScheduleBlockerDisplayPeriodID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ScheduleBlockers
	#'
	#' This function returns a dataframe or json object of ScheduleBlockers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ScheduleBlockers. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ScheduleBlockers.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ScheduleBlocker') to get more field paths.
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
	#' @concept Staff
	#' @return A list of ScheduleBlockers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listScheduleBlockers <- function(searchConditionsList = NULL, ScheduleBlockerID = F, StaffID = F, EntityID = F, SchoolYearID = F, StartDate = F, EndDate = F, Reason = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Staff", objectName = "ScheduleBlocker", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ScheduleBlocker
	#'
	#' This function returns a dataframe or json object of a ScheduleBlocker
	#' @param ScheduleBlockerID The ID of the ScheduleBlocker to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ScheduleBlocker. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ScheduleBlocker.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ScheduleBlocker') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return A dataframe or of ScheduleBlocker
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getScheduleBlocker <- function(ScheduleBlockerID, StaffID = F, EntityID = F, SchoolYearID = F, StartDate = F, EndDate = F, Reason = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ScheduleBlockerID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Staff", objectName = "ScheduleBlocker", objectId = ScheduleBlockerID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ScheduleBlocker
	#'
	#' This function deletes a ScheduleBlocker
	#' @param ScheduleBlockerID The ID of the ScheduleBlocker to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return The ScheduleBlockerID of the deleted ScheduleBlocker.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteScheduleBlocker <- function(ScheduleBlockerID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Staff", objectName = "ScheduleBlocker", objectId = ScheduleBlockerID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ScheduleBlocker
	#'
	#' This function creates a ScheduleBlocker
	#' @param fieldNames The field values to give the created ScheduleBlocker. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return A newly created ScheduleBlocker
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createScheduleBlocker <- function(StaffID = NULL, EntityID = NULL, SchoolYearID = NULL, StartDate = NULL, EndDate = NULL, Reason = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Staff", objectName = "ScheduleBlocker", body = list(DataObject = body), searchFields = append("ScheduleBlockerID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ScheduleBlocker
	#'
	#' This function modifies a ScheduleBlocker
	#' @param fieldNames The field values to give the modified ScheduleBlocker. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return The modified ScheduleBlocker
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyScheduleBlocker <- function(ScheduleBlockerID, StaffID = NULL, EntityID = NULL, SchoolYearID = NULL, StartDate = NULL, EndDate = NULL, Reason = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Staff", objectName = "ScheduleBlocker", objectId = ScheduleBlockerID, body = list(DataObject = body), searchFields = append("ScheduleBlockerID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List MassPrintStaffScheduleRunHistories
	#'
	#' This function returns a dataframe or json object of MassPrintStaffScheduleRunHistories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given MassPrintStaffScheduleRunHistories. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MassPrintStaffScheduleRunHistories.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MassPrintStaffScheduleRunHistory') to get more field paths.
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
	#' @concept Staff
	#' @return A list of MassPrintStaffScheduleRunHistories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listMassPrintStaffScheduleRunHistories <- function(searchConditionsList = NULL, MassPrintStaffScheduleRunHistoryID = F, EntityID = F, SchoolYearID = F, RunDescription = F, MediaID = F, WorkflowInstanceID = F, RequestIdentifier = F, SendMessageOnComplete = F, ParameterData = F, ParameterDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Staff", objectName = "MassPrintStaffScheduleRunHistory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a MassPrintStaffScheduleRunHistory
	#'
	#' This function returns a dataframe or json object of a MassPrintStaffScheduleRunHistory
	#' @param MassPrintStaffScheduleRunHistoryID The ID of the MassPrintStaffScheduleRunHistory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given MassPrintStaffScheduleRunHistory. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MassPrintStaffScheduleRunHistory.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MassPrintStaffScheduleRunHistory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return A dataframe or of MassPrintStaffScheduleRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getMassPrintStaffScheduleRunHistory <- function(MassPrintStaffScheduleRunHistoryID, EntityID = F, SchoolYearID = F, RunDescription = F, MediaID = F, WorkflowInstanceID = F, RequestIdentifier = F, SendMessageOnComplete = F, ParameterData = F, ParameterDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "MassPrintStaffScheduleRunHistoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Staff", objectName = "MassPrintStaffScheduleRunHistory", objectId = MassPrintStaffScheduleRunHistoryID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a MassPrintStaffScheduleRunHistory
	#'
	#' This function deletes a MassPrintStaffScheduleRunHistory
	#' @param MassPrintStaffScheduleRunHistoryID The ID of the MassPrintStaffScheduleRunHistory to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return The MassPrintStaffScheduleRunHistoryID of the deleted MassPrintStaffScheduleRunHistory.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteMassPrintStaffScheduleRunHistory <- function(MassPrintStaffScheduleRunHistoryID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Staff", objectName = "MassPrintStaffScheduleRunHistory", objectId = MassPrintStaffScheduleRunHistoryID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a MassPrintStaffScheduleRunHistory
	#'
	#' This function creates a MassPrintStaffScheduleRunHistory
	#' @param fieldNames The field values to give the created MassPrintStaffScheduleRunHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return A newly created MassPrintStaffScheduleRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createMassPrintStaffScheduleRunHistory <- function(EntityID = NULL, SchoolYearID = NULL, RunDescription = NULL, MediaID = NULL, WorkflowInstanceID = NULL, SendMessageOnComplete = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Staff", objectName = "MassPrintStaffScheduleRunHistory", body = list(DataObject = body), searchFields = append("MassPrintStaffScheduleRunHistoryID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a MassPrintStaffScheduleRunHistory
	#'
	#' This function modifies a MassPrintStaffScheduleRunHistory
	#' @param fieldNames The field values to give the modified MassPrintStaffScheduleRunHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return The modified MassPrintStaffScheduleRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyMassPrintStaffScheduleRunHistory <- function(MassPrintStaffScheduleRunHistoryID, EntityID = NULL, SchoolYearID = NULL, RunDescription = NULL, MediaID = NULL, WorkflowInstanceID = NULL, SendMessageOnComplete = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Staff", objectName = "MassPrintStaffScheduleRunHistory", objectId = MassPrintStaffScheduleRunHistoryID, body = list(DataObject = body), searchFields = append("MassPrintStaffScheduleRunHistoryID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StaffConfigEntityGroupYears
	#'
	#' This function returns a dataframe or json object of StaffConfigEntityGroupYears
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StaffConfigEntityGroupYears. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StaffConfigEntityGroupYears.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StaffConfigEntityGroupYear') to get more field paths.
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
	#' @concept Staff
	#' @return A list of StaffConfigEntityGroupYears
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStaffConfigEntityGroupYears <- function(searchConditionsList = NULL, ConfigEntityGroupYearID = F, SchoolYearID = F, EntityID = F, EntityGroupKey = F, AllowFoodServiceOnlinePaymentsAccessByDefault = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Staff", objectName = "ConfigEntityGroupYear", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StaffConfigEntityGroupYear
	#'
	#' This function returns a dataframe or json object of a StaffConfigEntityGroupYear
	#' @param StaffConfigEntityGroupYearID The ID of the StaffConfigEntityGroupYear to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StaffConfigEntityGroupYear. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StaffConfigEntityGroupYear.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StaffConfigEntityGroupYear') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return A dataframe or of StaffConfigEntityGroupYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStaffConfigEntityGroupYear <- function(StaffConfigEntityGroupYearID, ConfigEntityGroupYearID = F, SchoolYearID = F, EntityID = F, EntityGroupKey = F, AllowFoodServiceOnlinePaymentsAccessByDefault = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StaffConfigEntityGroupYearID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Staff", objectName = "ConfigEntityGroupYear", objectId = StaffConfigEntityGroupYearID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StaffConfigEntityGroupYear
	#'
	#' This function deletes a StaffConfigEntityGroupYear
	#' @param StaffConfigEntityGroupYearID The ID of the StaffConfigEntityGroupYear to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return The StaffConfigEntityGroupYearID of the deleted StaffConfigEntityGroupYear.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStaffConfigEntityGroupYear <- function(StaffConfigEntityGroupYearID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Staff", objectName = "ConfigEntityGroupYear", objectId = StaffConfigEntityGroupYearID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StaffConfigEntityGroupYear
	#'
	#' This function creates a StaffConfigEntityGroupYear
	#' @param fieldNames The field values to give the created StaffConfigEntityGroupYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return A newly created StaffConfigEntityGroupYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStaffConfigEntityGroupYear <- function(SchoolYearID = NULL, EntityID = NULL, EntityGroupKey = NULL, AllowFoodServiceOnlinePaymentsAccessByDefault = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Staff", objectName = "ConfigEntityGroupYear", body = list(DataObject = body), searchFields = append("ConfigEntityGroupYearID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StaffConfigEntityGroupYear
	#'
	#' This function modifies a StaffConfigEntityGroupYear
	#' @param fieldNames The field values to give the modified StaffConfigEntityGroupYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return The modified StaffConfigEntityGroupYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStaffConfigEntityGroupYear <- function(ConfigEntityGroupYearID, SchoolYearID = NULL, EntityID = NULL, EntityGroupKey = NULL, AllowFoodServiceOnlinePaymentsAccessByDefault = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Staff", objectName = "ConfigEntityGroupYear", objectId = ConfigEntityGroupYearID, body = list(DataObject = body), searchFields = append("ConfigEntityGroupYearID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List JobTitleMAS
	#'
	#' This function returns a dataframe or json object of JobTitleMAS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given JobTitleMAS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the JobTitleMAS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('JobTitleMA') to get more field paths.
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
	#' @concept Staff
	#' @return A list of JobTitleMAS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listJobTitleMAS <- function(searchConditionsList = NULL, JobTitleMAID = F, Title = F, StaffID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Staff", objectName = "JobTitleMA", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a JobTitleMA
	#'
	#' This function returns a dataframe or json object of a JobTitleMA
	#' @param JobTitleMAID The ID of the JobTitleMA to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given JobTitleMA. Defaults to FALSE for all return fields which, for convenience, returns all fields for the JobTitleMA.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('JobTitleMA') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return A dataframe or of JobTitleMA
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getJobTitleMA <- function(JobTitleMAID, Title = F, StaffID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "JobTitleMAID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Staff", objectName = "JobTitleMA", objectId = JobTitleMAID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a JobTitleMA
	#'
	#' This function deletes a JobTitleMA
	#' @param JobTitleMAID The ID of the JobTitleMA to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return The JobTitleMAID of the deleted JobTitleMA.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteJobTitleMA <- function(JobTitleMAID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Staff", objectName = "JobTitleMA", objectId = JobTitleMAID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a JobTitleMA
	#'
	#' This function creates a JobTitleMA
	#' @param fieldNames The field values to give the created JobTitleMA. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return A newly created JobTitleMA
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createJobTitleMA <- function(Title = NULL, StaffID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Staff", objectName = "JobTitleMA", body = list(DataObject = body), searchFields = append("JobTitleMAID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a JobTitleMA
	#'
	#' This function modifies a JobTitleMA
	#' @param fieldNames The field values to give the modified JobTitleMA. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return The modified JobTitleMA
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyJobTitleMA <- function(JobTitleMAID, Title = NULL, StaffID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Staff", objectName = "JobTitleMA", objectId = JobTitleMAID, body = list(DataObject = body), searchFields = append("JobTitleMAID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List NumberedStaffEntityYearForDistrictAndSchoolYears
	#'
	#' This function returns a dataframe or json object of NumberedStaffEntityYearForDistrictAndSchoolYears
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NumberedStaffEntityYearForDistrictAndSchoolYears. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NumberedStaffEntityYearForDistrictAndSchoolYears.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NumberedStaffEntityYearForDistrictAndSchoolYear') to get more field paths.
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
	#' @concept Staff
	#' @return A list of NumberedStaffEntityYearForDistrictAndSchoolYears
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNumberedStaffEntityYearForDistrictAndSchoolYears <- function(searchConditionsList = NULL, DistrictID = F, EntityID = F, SchoolYearID = F, StaffDistrictRowNumber = F, StaffID = F, StaffEntityYearID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Staff", objectName = "NumberedStaffEntityYearForDistrictAndSchoolYear", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NumberedStaffEntityYearForDistrictAndSchoolYear
	#'
	#' This function returns a dataframe or json object of a NumberedStaffEntityYearForDistrictAndSchoolYear
	#' @param NumberedStaffEntityYearForDistrictAndSchoolYearID The ID of the NumberedStaffEntityYearForDistrictAndSchoolYear to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NumberedStaffEntityYearForDistrictAndSchoolYear. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NumberedStaffEntityYearForDistrictAndSchoolYear.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NumberedStaffEntityYearForDistrictAndSchoolYear') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return A dataframe or of NumberedStaffEntityYearForDistrictAndSchoolYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNumberedStaffEntityYearForDistrictAndSchoolYear <- function(NumberedStaffEntityYearForDistrictAndSchoolYearID, DistrictID = F, EntityID = F, SchoolYearID = F, StaffDistrictRowNumber = F, StaffID = F, StaffEntityYearID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NumberedStaffEntityYearForDistrictAndSchoolYearID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Staff", objectName = "NumberedStaffEntityYearForDistrictAndSchoolYear", objectId = NumberedStaffEntityYearForDistrictAndSchoolYearID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NumberedStaffEntityYearForDistrictAndSchoolYear
	#'
	#' This function deletes a NumberedStaffEntityYearForDistrictAndSchoolYear
	#' @param NumberedStaffEntityYearForDistrictAndSchoolYearID The ID of the NumberedStaffEntityYearForDistrictAndSchoolYear to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff
	#' @return The NumberedStaffEntityYearForDistrictAndSchoolYearID of the deleted NumberedStaffEntityYearForDistrictAndSchoolYear.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNumberedStaffEntityYearForDistrictAndSchoolYear <- function(NumberedStaffEntityYearForDistrictAndSchoolYearID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Staff", objectName = "NumberedStaffEntityYearForDistrictAndSchoolYear", objectId = NumberedStaffEntityYearForDistrictAndSchoolYearID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}
