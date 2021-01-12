
	#' List EnrollmentConfigDistrictYears
	#'
	#' This function returns a dataframe or json object of EnrollmentConfigDistrictYears
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EnrollmentConfigDistrictYears. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EnrollmentConfigDistrictYears.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EnrollmentConfigDistrictYear') to get more field paths.
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
	#' @concept Enrollment
	#' @return A list of EnrollmentConfigDistrictYears
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEnrollmentConfigDistrictYears <- function(searchConditionsList = NULL, AutoAddSchoolPathOverride = F, ConfigDistrictYearID = F, ConfigDistrictYearIDClonedFrom = F, CreatedTime = F, DefaultRetainInterventionPlanRecords = F, DistrictID = F, EnableNoShow = F, EnrolledDifferentEntityNoShowActionType = F, EnrolledDifferentEntityNoShowEntryDate = F, EnrolledDifferentEntityNoShowWithdrawalDate = F, ModifiedTime = F, NoDistrictEnrollmentNoShowActionType = F, NoDistrictEnrollmentNoShowEntryDate = F, NoDistrictEnrollmentNoShowWithdrawalDate = F, PermitIDAutoAdd = F, PreviouslyEnrolledSameEntityNoShowActionType = F, PreviouslyEnrolledSameEntityNoShowEntryDate = F, PreviouslyEnrolledSameEntityNoShowWithdrawalDate = F, PriorNoShowRecord = F, SchoolYearID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "ConfigDistrictYear", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EnrollmentConfigDistrictYear
	#'
	#' This function returns a dataframe or json object of an EnrollmentConfigDistrictYear
	#' @param EnrollmentConfigDistrictYearID The ID of the EnrollmentConfigDistrictYear to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EnrollmentConfigDistrictYear. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EnrollmentConfigDistrictYear.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EnrollmentConfigDistrictYear') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A dataframe or of EnrollmentConfigDistrictYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEnrollmentConfigDistrictYear <- function(EnrollmentConfigDistrictYearID, AutoAddSchoolPathOverride = F, ConfigDistrictYearID = F, ConfigDistrictYearIDClonedFrom = F, CreatedTime = F, DefaultRetainInterventionPlanRecords = F, DistrictID = F, EnableNoShow = F, EnrolledDifferentEntityNoShowActionType = F, EnrolledDifferentEntityNoShowEntryDate = F, EnrolledDifferentEntityNoShowWithdrawalDate = F, ModifiedTime = F, NoDistrictEnrollmentNoShowActionType = F, NoDistrictEnrollmentNoShowEntryDate = F, NoDistrictEnrollmentNoShowWithdrawalDate = F, PermitIDAutoAdd = F, PreviouslyEnrolledSameEntityNoShowActionType = F, PreviouslyEnrolledSameEntityNoShowEntryDate = F, PreviouslyEnrolledSameEntityNoShowWithdrawalDate = F, PriorNoShowRecord = F, SchoolYearID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EnrollmentConfigDistrictYearID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Enrollment", objectName = "ConfigDistrictYear", objectId = EnrollmentConfigDistrictYearID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EnrollmentConfigDistrictYear
	#'
	#' This function deletes an EnrollmentConfigDistrictYear
	#' @param EnrollmentConfigDistrictYearID The ID of the EnrollmentConfigDistrictYear to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The EnrollmentConfigDistrictYearID of the deleted EnrollmentConfigDistrictYear.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEnrollmentConfigDistrictYear <- function(EnrollmentConfigDistrictYearID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Enrollment", objectName = "ConfigDistrictYear", objectId = EnrollmentConfigDistrictYearID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EnrollmentConfigDistrictYear
	#'
	#' This function creates an EnrollmentConfigDistrictYear
	#' @param fieldNames The field values to give the created EnrollmentConfigDistrictYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A newly created EnrollmentConfigDistrictYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEnrollmentConfigDistrictYear <- function(AutoAddSchoolPathOverride = NULL, ConfigDistrictYearIDClonedFrom = NULL, DefaultRetainInterventionPlanRecords = NULL, DistrictID = NULL, EnableNoShow = NULL, EnrolledDifferentEntityNoShowActionType = NULL, EnrolledDifferentEntityNoShowEntryDate = NULL, EnrolledDifferentEntityNoShowWithdrawalDate = NULL, NoDistrictEnrollmentNoShowActionType = NULL, NoDistrictEnrollmentNoShowEntryDate = NULL, NoDistrictEnrollmentNoShowWithdrawalDate = NULL, PermitIDAutoAdd = NULL, PreviouslyEnrolledSameEntityNoShowActionType = NULL, PreviouslyEnrolledSameEntityNoShowEntryDate = NULL, PreviouslyEnrolledSameEntityNoShowWithdrawalDate = NULL, PriorNoShowRecord = NULL, SchoolYearID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Enrollment", objectName = "ConfigDistrictYear", body = list(DataObject = body), searchFields = append("ConfigDistrictYearID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EnrollmentConfigDistrictYear
	#'
	#' This function modifies an EnrollmentConfigDistrictYear
	#' @param fieldNames The field values to give the modified EnrollmentConfigDistrictYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The modified EnrollmentConfigDistrictYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEnrollmentConfigDistrictYear <- function(ConfigDistrictYearID, AutoAddSchoolPathOverride = NULL, ConfigDistrictYearIDClonedFrom = NULL, DefaultRetainInterventionPlanRecords = NULL, DistrictID = NULL, EnableNoShow = NULL, EnrolledDifferentEntityNoShowActionType = NULL, EnrolledDifferentEntityNoShowEntryDate = NULL, EnrolledDifferentEntityNoShowWithdrawalDate = NULL, NoDistrictEnrollmentNoShowActionType = NULL, NoDistrictEnrollmentNoShowEntryDate = NULL, NoDistrictEnrollmentNoShowWithdrawalDate = NULL, PermitIDAutoAdd = NULL, PreviouslyEnrolledSameEntityNoShowActionType = NULL, PreviouslyEnrolledSameEntityNoShowEntryDate = NULL, PreviouslyEnrolledSameEntityNoShowWithdrawalDate = NULL, PriorNoShowRecord = NULL, SchoolYearID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Enrollment", objectName = "ConfigDistrictYear", objectId = ConfigDistrictYearID, body = list(DataObject = body), searchFields = append("ConfigDistrictYearID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EnrollmentConfigDistrictYearWithdrawalCodes
	#'
	#' This function returns a dataframe or json object of EnrollmentConfigDistrictYearWithdrawalCodes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EnrollmentConfigDistrictYearWithdrawalCodes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EnrollmentConfigDistrictYearWithdrawalCodes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EnrollmentConfigDistrictYearWithdrawalCode') to get more field paths.
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
	#' @concept Enrollment
	#' @return A list of EnrollmentConfigDistrictYearWithdrawalCodes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEnrollmentConfigDistrictYearWithdrawalCodes <- function(searchConditionsList = NULL, ConfigDistrictYearID = F, ConfigDistrictYearWithdrawalCodeID = F, ConfigDistrictYearWithdrawalCodeIDClonedFrom = F, CreatedTime = F, ModifiedTime = F, Type = F, UserIDCreatedBy = F, UserIDModifiedBy = F, WithdrawalCodeID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "ConfigDistrictYearWithdrawalCode", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EnrollmentConfigDistrictYearWithdrawalCode
	#'
	#' This function returns a dataframe or json object of an EnrollmentConfigDistrictYearWithdrawalCode
	#' @param EnrollmentConfigDistrictYearWithdrawalCodeID The ID of the EnrollmentConfigDistrictYearWithdrawalCode to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EnrollmentConfigDistrictYearWithdrawalCode. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EnrollmentConfigDistrictYearWithdrawalCode.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EnrollmentConfigDistrictYearWithdrawalCode') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A dataframe or of EnrollmentConfigDistrictYearWithdrawalCode
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEnrollmentConfigDistrictYearWithdrawalCode <- function(EnrollmentConfigDistrictYearWithdrawalCodeID, ConfigDistrictYearID = F, ConfigDistrictYearWithdrawalCodeID = F, ConfigDistrictYearWithdrawalCodeIDClonedFrom = F, CreatedTime = F, ModifiedTime = F, Type = F, UserIDCreatedBy = F, UserIDModifiedBy = F, WithdrawalCodeID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EnrollmentConfigDistrictYearWithdrawalCodeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Enrollment", objectName = "ConfigDistrictYearWithdrawalCode", objectId = EnrollmentConfigDistrictYearWithdrawalCodeID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EnrollmentConfigDistrictYearWithdrawalCode
	#'
	#' This function deletes an EnrollmentConfigDistrictYearWithdrawalCode
	#' @param EnrollmentConfigDistrictYearWithdrawalCodeID The ID of the EnrollmentConfigDistrictYearWithdrawalCode to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The EnrollmentConfigDistrictYearWithdrawalCodeID of the deleted EnrollmentConfigDistrictYearWithdrawalCode.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEnrollmentConfigDistrictYearWithdrawalCode <- function(EnrollmentConfigDistrictYearWithdrawalCodeID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Enrollment", objectName = "ConfigDistrictYearWithdrawalCode", objectId = EnrollmentConfigDistrictYearWithdrawalCodeID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EnrollmentConfigDistrictYearWithdrawalCode
	#'
	#' This function creates an EnrollmentConfigDistrictYearWithdrawalCode
	#' @param fieldNames The field values to give the created EnrollmentConfigDistrictYearWithdrawalCode. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A newly created EnrollmentConfigDistrictYearWithdrawalCode
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEnrollmentConfigDistrictYearWithdrawalCode <- function(ConfigDistrictYearID = NULL, ConfigDistrictYearWithdrawalCodeIDClonedFrom = NULL, Type = NULL, WithdrawalCodeID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Enrollment", objectName = "ConfigDistrictYearWithdrawalCode", body = list(DataObject = body), searchFields = append("ConfigDistrictYearWithdrawalCodeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EnrollmentConfigDistrictYearWithdrawalCode
	#'
	#' This function modifies an EnrollmentConfigDistrictYearWithdrawalCode
	#' @param fieldNames The field values to give the modified EnrollmentConfigDistrictYearWithdrawalCode. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The modified EnrollmentConfigDistrictYearWithdrawalCode
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEnrollmentConfigDistrictYearWithdrawalCode <- function(ConfigDistrictYearWithdrawalCodeID, ConfigDistrictYearID = NULL, ConfigDistrictYearWithdrawalCodeIDClonedFrom = NULL, Type = NULL, WithdrawalCodeID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Enrollment", objectName = "ConfigDistrictYearWithdrawalCode", objectId = ConfigDistrictYearWithdrawalCodeID, body = list(DataObject = body), searchFields = append("ConfigDistrictYearWithdrawalCodeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempStudentEntityYears
	#'
	#' This function returns a dataframe or json object of TempStudentEntityYears
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentEntityYears. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentEntityYears.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentEntityYear') to get more field paths.
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
	#' @concept Enrollment
	#' @return A list of TempStudentEntityYears
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStudentEntityYears <- function(searchConditionsList = NULL, AdvisorDetails = F, CreatedTime = F, CurrentAdvisorDetails = F, CurrentHomeroomDetails = F, GenderCode = F, GradeLevelCodeDescription = F, HomeroomDetails = F, HomeroomID = F, IsActive = F, Message = F, ModifiedTime = F, StaffIDAdvisor = F, StudentEntityYearID = F, StudentFullName = F, StudentNumber = F, TempStudentEntityYearID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "TempStudentEntityYear", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempStudentEntityYear
	#'
	#' This function returns a dataframe or json object of a TempStudentEntityYear
	#' @param TempStudentEntityYearID The ID of the TempStudentEntityYear to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentEntityYear. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentEntityYear.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentEntityYear') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A dataframe or of TempStudentEntityYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempStudentEntityYear <- function(TempStudentEntityYearID, AdvisorDetails = F, CreatedTime = F, CurrentAdvisorDetails = F, CurrentHomeroomDetails = F, GenderCode = F, GradeLevelCodeDescription = F, HomeroomDetails = F, HomeroomID = F, IsActive = F, Message = F, ModifiedTime = F, StaffIDAdvisor = F, StudentEntityYearID = F, StudentFullName = F, StudentNumber = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempStudentEntityYearID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Enrollment", objectName = "TempStudentEntityYear", objectId = TempStudentEntityYearID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempStudentEntityYear
	#'
	#' This function deletes a TempStudentEntityYear
	#' @param TempStudentEntityYearID The ID of the TempStudentEntityYear to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The TempStudentEntityYearID of the deleted TempStudentEntityYear.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempStudentEntityYear <- function(TempStudentEntityYearID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Enrollment", objectName = "TempStudentEntityYear", objectId = TempStudentEntityYearID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempStudentEntityYear
	#'
	#' This function creates a TempStudentEntityYear
	#' @param fieldNames The field values to give the created TempStudentEntityYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A newly created TempStudentEntityYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempStudentEntityYear <- function(AdvisorDetails = NULL, CurrentAdvisorDetails = NULL, CurrentHomeroomDetails = NULL, GenderCode = NULL, GradeLevelCodeDescription = NULL, HomeroomDetails = NULL, HomeroomID = NULL, IsActive = NULL, Message = NULL, StaffIDAdvisor = NULL, StudentEntityYearID = NULL, StudentFullName = NULL, StudentNumber = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Enrollment", objectName = "TempStudentEntityYear", body = list(DataObject = body), searchFields = append("TempStudentEntityYearID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempStudentEntityYear
	#'
	#' This function modifies a TempStudentEntityYear
	#' @param fieldNames The field values to give the modified TempStudentEntityYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The modified TempStudentEntityYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempStudentEntityYear <- function(TempStudentEntityYearID, AdvisorDetails = NULL, CurrentAdvisorDetails = NULL, CurrentHomeroomDetails = NULL, GenderCode = NULL, GradeLevelCodeDescription = NULL, HomeroomDetails = NULL, HomeroomID = NULL, IsActive = NULL, Message = NULL, StaffIDAdvisor = NULL, StudentEntityYearID = NULL, StudentFullName = NULL, StudentNumber = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Enrollment", objectName = "TempStudentEntityYear", objectId = TempStudentEntityYearID, body = list(DataObject = body), searchFields = append("TempStudentEntityYearID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EntryWithdrawals
	#'
	#' This function returns a dataframe or json object of EntryWithdrawals
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EntryWithdrawals. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EntryWithdrawals.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EntryWithdrawal') to get more field paths.
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
	#' @concept Enrollment
	#' @return A list of EntryWithdrawals
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEntryWithdrawals <- function(searchConditionsList = NULL, AttendanceDays = F, CalendarID = F, CreatedTime = F, EndDate = F, EnrolledAtLeastOneDay = F, EntityID = F, EntryCodeID = F, EntryComment = F, EntryWithdrawalID = F, EntryWithdrawalIDStatusChangePrevious = F, EntryWithdrawalMNID = F, GradeReferenceID = F, HasMessageCenterAllowedWithdrawalCodeOverride = F, IsCombinedEnrollmentFullTime = F, IsCrossEntityCourseEnrollment = F, IsCurrentOrFutureEnrollment = F, IsDefaultEntity = F, IsHistoricalEnrollment = F, IsIndependentStudy = F, IsNoShow = F, IsPostSecondaryOption = F, IsPSEOConcurrentEnrollment = F, IsStartDateOnOrAfterFirstDayOfSchool = F, MembershipDays = F, ModifiedTime = F, PercentEnrolled = F, PromotionStatus = F, PSEOHours = F, RenderDeleteOption = F, RenderNoShowOption = F, RenderPrintWithdrawalFormOption = F, RenderStatusChangeOption = F, RenderUndoStatusChangeOption = F, RenderWithdrawalOption = F, SchoolID = F, SchoolYearID = F, SpecialEdServiceHours = F, StartDate = F, StateAidCategoryCodeMNID = F, StateDistrictMNID = F, StateLastAttendanceLocationCodeMNID = F, StatusChangeEntry = F, StatusChangeWithdrawal = F, StudentID = F, StudentTypeID = F, TotalMembershipDays = F, UserIDCreatedBy = F, UserIDModifiedBy = F, WithdrawalCodeID = F, WithdrawalComment = F, WithdrawalDate = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "EntryWithdrawal", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EntryWithdrawal
	#'
	#' This function returns a dataframe or json object of an EntryWithdrawal
	#' @param EntryWithdrawalID The ID of the EntryWithdrawal to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EntryWithdrawal. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EntryWithdrawal.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EntryWithdrawal') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A dataframe or of EntryWithdrawal
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEntryWithdrawal <- function(EntryWithdrawalID, AttendanceDays = F, CalendarID = F, CreatedTime = F, EndDate = F, EnrolledAtLeastOneDay = F, EntityID = F, EntryCodeID = F, EntryComment = F, EntryWithdrawalIDStatusChangePrevious = F, EntryWithdrawalMNID = F, GradeReferenceID = F, HasMessageCenterAllowedWithdrawalCodeOverride = F, IsCombinedEnrollmentFullTime = F, IsCrossEntityCourseEnrollment = F, IsCurrentOrFutureEnrollment = F, IsDefaultEntity = F, IsHistoricalEnrollment = F, IsIndependentStudy = F, IsNoShow = F, IsPostSecondaryOption = F, IsPSEOConcurrentEnrollment = F, IsStartDateOnOrAfterFirstDayOfSchool = F, MembershipDays = F, ModifiedTime = F, PercentEnrolled = F, PromotionStatus = F, PSEOHours = F, RenderDeleteOption = F, RenderNoShowOption = F, RenderPrintWithdrawalFormOption = F, RenderStatusChangeOption = F, RenderUndoStatusChangeOption = F, RenderWithdrawalOption = F, SchoolID = F, SchoolYearID = F, SpecialEdServiceHours = F, StartDate = F, StateAidCategoryCodeMNID = F, StateDistrictMNID = F, StateLastAttendanceLocationCodeMNID = F, StatusChangeEntry = F, StatusChangeWithdrawal = F, StudentID = F, StudentTypeID = F, TotalMembershipDays = F, UserIDCreatedBy = F, UserIDModifiedBy = F, WithdrawalCodeID = F, WithdrawalComment = F, WithdrawalDate = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EntryWithdrawalID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Enrollment", objectName = "EntryWithdrawal", objectId = EntryWithdrawalID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EntryWithdrawal
	#'
	#' This function deletes an EntryWithdrawal
	#' @param EntryWithdrawalID The ID of the EntryWithdrawal to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The EntryWithdrawalID of the deleted EntryWithdrawal.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEntryWithdrawal <- function(EntryWithdrawalID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Enrollment", objectName = "EntryWithdrawal", objectId = EntryWithdrawalID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EntryWithdrawal
	#'
	#' This function creates an EntryWithdrawal
	#' @param fieldNames The field values to give the created EntryWithdrawal. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A newly created EntryWithdrawal
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEntryWithdrawal <- function(AttendanceDays = NULL, CalendarID = NULL, EndDate = NULL, EntityID = NULL, EntryCodeID = NULL, EntryComment = NULL, EntryWithdrawalIDStatusChangePrevious = NULL, GradeReferenceID = NULL, IsDefaultEntity = NULL, IsIndependentStudy = NULL, IsNoShow = NULL, IsPostSecondaryOption = NULL, IsPSEOConcurrentEnrollment = NULL, MembershipDays = NULL, PercentEnrolled = NULL, PromotionStatus = NULL, PSEOHours = NULL, SchoolID = NULL, SchoolYearID = NULL, SpecialEdServiceHours = NULL, StartDate = NULL, StateAidCategoryCodeMNID = NULL, StateDistrictMNID = NULL, StateLastAttendanceLocationCodeMNID = NULL, StudentID = NULL, StudentTypeID = NULL, WithdrawalCodeID = NULL, WithdrawalComment = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Enrollment", objectName = "EntryWithdrawal", body = list(DataObject = body), searchFields = append("EntryWithdrawalID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EntryWithdrawal
	#'
	#' This function modifies an EntryWithdrawal
	#' @param fieldNames The field values to give the modified EntryWithdrawal. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The modified EntryWithdrawal
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEntryWithdrawal <- function(EntryWithdrawalID, AttendanceDays = NULL, CalendarID = NULL, EndDate = NULL, EntityID = NULL, EntryCodeID = NULL, EntryComment = NULL, EntryWithdrawalIDStatusChangePrevious = NULL, GradeReferenceID = NULL, IsDefaultEntity = NULL, IsIndependentStudy = NULL, IsNoShow = NULL, IsPostSecondaryOption = NULL, IsPSEOConcurrentEnrollment = NULL, MembershipDays = NULL, PercentEnrolled = NULL, PromotionStatus = NULL, PSEOHours = NULL, SchoolID = NULL, SchoolYearID = NULL, SpecialEdServiceHours = NULL, StartDate = NULL, StateAidCategoryCodeMNID = NULL, StateDistrictMNID = NULL, StateLastAttendanceLocationCodeMNID = NULL, StudentID = NULL, StudentTypeID = NULL, WithdrawalCodeID = NULL, WithdrawalComment = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Enrollment", objectName = "EntryWithdrawal", objectId = EntryWithdrawalID, body = list(DataObject = body), searchFields = append("EntryWithdrawalID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Schools
	#'
	#' This function returns a dataframe or json object of Schools
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Schools. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Schools.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('School') to get more field paths.
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
	#' @concept Enrollment
	#' @return A list of Schools
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSchools <- function(searchConditionsList = NULL, AllowsSchoolDevices = F, AllowsStudentDevices = F, BuildingID = F, CampusAccountabilityRatingID = F, CEEBCode = F, Code = F, CodeName = F, CreatedTime = F, DaysInRegularSchoolYear = F, DaysPriorForAlgebraICounts = F, DistrictID = F, EdFiSchoolCategoryID = F, EdFiSchoolID = F, EducationalProgramHoursPerWeek = F, ExcludeFromCRDC = F, FaxNumber = F, FaxNumberIsInternational = F, FederalAlternativeSchoolDetailID = F, FederalJusticeFacilityTypeID = F, FederalNCESSchoolID = F, FormattedFaxNumber = F, FormattedPhoneNumber = F, GradeLevelIDHigh = F, GradeLevelIDLow = F, HasAlcoholDrugEducation = F, HasAntiBullying = F, HasAntiViolence = F, HasAPCourses = F, HasAPSelfSelection = F, HasCorporalPunishment = F, HasCreditRecovery = F, HasCrisisPlan = F, HasDualEnrollment = F, HasFiberOptic = F, HasGifted = F, HasHomicideOccurred = F, HasIBDiplomaProgramme = F, HasPreschoolNonIDEAAge3 = F, HasPreschoolNonIDEAAge4 = F, HasPreschoolNonIDEAAge5 = F, HasSafetyPlan = F, HasShootingOccurred = F, HasSingleSexAthletics = F, HasSingleSexClasses = F, HasUngraded = F, HasUngradedMainlyElementary = F, HasUngradedMainlyHighSchool = F, HasUngradedMainlyMiddleSchool = F, HasWiFi = F, HasZeroTolerance = F, IsALCSchool = F, IsAlternative = F, IsCEP = F, IsCharter = F, IsCRDCCollectedForSchoolYear = F, IsEntireSchoolMagnet = F, IsMagnet = F, IsNonLEA = F, IsSpecialEducation = F, IsTitleIII = F, IsTitleISchoolwide = F, ModifiedTime = F, Name = F, NameIDSafetySpecialist = F, NumberWiFiDevices = F, PhoneNumber = F, PhoneNumberIsInternational = F, SchoolID = F, SchoolIDClonedFrom = F, SchoolIDClonedTo = F, SchoolMNID = F, SchoolNumber = F, SchoolYearID = F, StaffIDPrincipal = F, StateAssignedID = F, StateKindergartenScheduleIndicatorCodeMNID = F, StateSchoolMNID = F, StateTitleISchoolIndicatorCodeMNID = F, Type = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "School", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a School
	#'
	#' This function returns a dataframe or json object of a School
	#' @param SchoolID The ID of the School to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given School. Defaults to FALSE for all return fields which, for convenience, returns all fields for the School.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('School') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A dataframe or of School
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSchool <- function(SchoolID, AllowsSchoolDevices = F, AllowsStudentDevices = F, BuildingID = F, CampusAccountabilityRatingID = F, CEEBCode = F, Code = F, CodeName = F, CreatedTime = F, DaysInRegularSchoolYear = F, DaysPriorForAlgebraICounts = F, DistrictID = F, EdFiSchoolCategoryID = F, EdFiSchoolID = F, EducationalProgramHoursPerWeek = F, ExcludeFromCRDC = F, FaxNumber = F, FaxNumberIsInternational = F, FederalAlternativeSchoolDetailID = F, FederalJusticeFacilityTypeID = F, FederalNCESSchoolID = F, FormattedFaxNumber = F, FormattedPhoneNumber = F, GradeLevelIDHigh = F, GradeLevelIDLow = F, HasAlcoholDrugEducation = F, HasAntiBullying = F, HasAntiViolence = F, HasAPCourses = F, HasAPSelfSelection = F, HasCorporalPunishment = F, HasCreditRecovery = F, HasCrisisPlan = F, HasDualEnrollment = F, HasFiberOptic = F, HasGifted = F, HasHomicideOccurred = F, HasIBDiplomaProgramme = F, HasPreschoolNonIDEAAge3 = F, HasPreschoolNonIDEAAge4 = F, HasPreschoolNonIDEAAge5 = F, HasSafetyPlan = F, HasShootingOccurred = F, HasSingleSexAthletics = F, HasSingleSexClasses = F, HasUngraded = F, HasUngradedMainlyElementary = F, HasUngradedMainlyHighSchool = F, HasUngradedMainlyMiddleSchool = F, HasWiFi = F, HasZeroTolerance = F, IsALCSchool = F, IsAlternative = F, IsCEP = F, IsCharter = F, IsCRDCCollectedForSchoolYear = F, IsEntireSchoolMagnet = F, IsMagnet = F, IsNonLEA = F, IsSpecialEducation = F, IsTitleIII = F, IsTitleISchoolwide = F, ModifiedTime = F, Name = F, NameIDSafetySpecialist = F, NumberWiFiDevices = F, PhoneNumber = F, PhoneNumberIsInternational = F, SchoolIDClonedFrom = F, SchoolIDClonedTo = F, SchoolMNID = F, SchoolNumber = F, SchoolYearID = F, StaffIDPrincipal = F, StateAssignedID = F, StateKindergartenScheduleIndicatorCodeMNID = F, StateSchoolMNID = F, StateTitleISchoolIndicatorCodeMNID = F, Type = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SchoolID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Enrollment", objectName = "School", objectId = SchoolID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a School
	#'
	#' This function deletes a School
	#' @param SchoolID The ID of the School to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The SchoolID of the deleted School.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSchool <- function(SchoolID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Enrollment", objectName = "School", objectId = SchoolID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a School
	#'
	#' This function creates a School
	#' @param fieldNames The field values to give the created School. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A newly created School
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSchool <- function(AllowsSchoolDevices = NULL, AllowsStudentDevices = NULL, CampusAccountabilityRatingID = NULL, CEEBCode = NULL, Code = NULL, DaysInRegularSchoolYear = NULL, DaysPriorForAlgebraICounts = NULL, DistrictID = NULL, EdFiSchoolCategoryID = NULL, EdFiSchoolID = NULL, EducationalProgramHoursPerWeek = NULL, ExcludeFromCRDC = NULL, FaxNumber = NULL, FaxNumberIsInternational = NULL, FederalAlternativeSchoolDetailID = NULL, FederalJusticeFacilityTypeID = NULL, FederalNCESSchoolID = NULL, GradeLevelIDHigh = NULL, GradeLevelIDLow = NULL, HasAlcoholDrugEducation = NULL, HasAntiBullying = NULL, HasAntiViolence = NULL, HasAPCourses = NULL, HasAPSelfSelection = NULL, HasCorporalPunishment = NULL, HasCreditRecovery = NULL, HasCrisisPlan = NULL, HasDualEnrollment = NULL, HasFiberOptic = NULL, HasGifted = NULL, HasHomicideOccurred = NULL, HasIBDiplomaProgramme = NULL, HasPreschoolNonIDEAAge3 = NULL, HasPreschoolNonIDEAAge4 = NULL, HasPreschoolNonIDEAAge5 = NULL, HasSafetyPlan = NULL, HasShootingOccurred = NULL, HasSingleSexAthletics = NULL, HasSingleSexClasses = NULL, HasUngraded = NULL, HasUngradedMainlyElementary = NULL, HasUngradedMainlyHighSchool = NULL, HasUngradedMainlyMiddleSchool = NULL, HasWiFi = NULL, HasZeroTolerance = NULL, IsALCSchool = NULL, IsAlternative = NULL, IsCEP = NULL, IsCharter = NULL, IsEntireSchoolMagnet = NULL, IsMagnet = NULL, IsNonLEA = NULL, IsSpecialEducation = NULL, IsTitleIII = NULL, IsTitleISchoolwide = NULL, Name = NULL, NameIDSafetySpecialist = NULL, NumberWiFiDevices = NULL, PhoneNumber = NULL, PhoneNumberIsInternational = NULL, SchoolIDClonedFrom = NULL, SchoolNumber = NULL, SchoolYearID = NULL, StaffIDPrincipal = NULL, StateKindergartenScheduleIndicatorCodeMNID = NULL, StateSchoolMNID = NULL, StateTitleISchoolIndicatorCodeMNID = NULL, Type = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Enrollment", objectName = "School", body = list(DataObject = body), searchFields = append("SchoolID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a School
	#'
	#' This function modifies a School
	#' @param fieldNames The field values to give the modified School. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The modified School
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySchool <- function(SchoolID, AllowsSchoolDevices = NULL, AllowsStudentDevices = NULL, CampusAccountabilityRatingID = NULL, CEEBCode = NULL, Code = NULL, DaysInRegularSchoolYear = NULL, DaysPriorForAlgebraICounts = NULL, DistrictID = NULL, EdFiSchoolCategoryID = NULL, EdFiSchoolID = NULL, EducationalProgramHoursPerWeek = NULL, ExcludeFromCRDC = NULL, FaxNumber = NULL, FaxNumberIsInternational = NULL, FederalAlternativeSchoolDetailID = NULL, FederalJusticeFacilityTypeID = NULL, FederalNCESSchoolID = NULL, GradeLevelIDHigh = NULL, GradeLevelIDLow = NULL, HasAlcoholDrugEducation = NULL, HasAntiBullying = NULL, HasAntiViolence = NULL, HasAPCourses = NULL, HasAPSelfSelection = NULL, HasCorporalPunishment = NULL, HasCreditRecovery = NULL, HasCrisisPlan = NULL, HasDualEnrollment = NULL, HasFiberOptic = NULL, HasGifted = NULL, HasHomicideOccurred = NULL, HasIBDiplomaProgramme = NULL, HasPreschoolNonIDEAAge3 = NULL, HasPreschoolNonIDEAAge4 = NULL, HasPreschoolNonIDEAAge5 = NULL, HasSafetyPlan = NULL, HasShootingOccurred = NULL, HasSingleSexAthletics = NULL, HasSingleSexClasses = NULL, HasUngraded = NULL, HasUngradedMainlyElementary = NULL, HasUngradedMainlyHighSchool = NULL, HasUngradedMainlyMiddleSchool = NULL, HasWiFi = NULL, HasZeroTolerance = NULL, IsALCSchool = NULL, IsAlternative = NULL, IsCEP = NULL, IsCharter = NULL, IsEntireSchoolMagnet = NULL, IsMagnet = NULL, IsNonLEA = NULL, IsSpecialEducation = NULL, IsTitleIII = NULL, IsTitleISchoolwide = NULL, Name = NULL, NameIDSafetySpecialist = NULL, NumberWiFiDevices = NULL, PhoneNumber = NULL, PhoneNumberIsInternational = NULL, SchoolIDClonedFrom = NULL, SchoolNumber = NULL, SchoolYearID = NULL, StaffIDPrincipal = NULL, StateKindergartenScheduleIndicatorCodeMNID = NULL, StateSchoolMNID = NULL, StateTitleISchoolIndicatorCodeMNID = NULL, Type = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Enrollment", objectName = "School", objectId = SchoolID, body = list(DataObject = body), searchFields = append("SchoolID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List WithdrawalCodes
	#'
	#' This function returns a dataframe or json object of WithdrawalCodes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given WithdrawalCodes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the WithdrawalCodes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('WithdrawalCode') to get more field paths.
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
	#' @concept Enrollment
	#' @return A list of WithdrawalCodes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listWithdrawalCodes <- function(searchConditionsList = NULL, Code = F, CodeDescription = F, CreatedTime = F, Description = F, DistrictGroupKey = F, DistrictID = F, EdFiExitWithdrawID = F, EdFiExitWithdrawTypeDescriptorID = F, IsCrossEntityCourseEnrollment = F, ModifiedTime = F, SchoolYearID = F, StateStatusEndCodeMNID = F, Type = F, UserIDCreatedBy = F, UserIDModifiedBy = F, WithdrawalCodeID = F, WithdrawalCodeIDClonedFrom = F, WithdrawalCodeMNID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "WithdrawalCode", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a WithdrawalCode
	#'
	#' This function returns a dataframe or json object of a WithdrawalCode
	#' @param WithdrawalCodeID The ID of the WithdrawalCode to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given WithdrawalCode. Defaults to FALSE for all return fields which, for convenience, returns all fields for the WithdrawalCode.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('WithdrawalCode') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A dataframe or of WithdrawalCode
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getWithdrawalCode <- function(WithdrawalCodeID, Code = F, CodeDescription = F, CreatedTime = F, Description = F, DistrictGroupKey = F, DistrictID = F, EdFiExitWithdrawID = F, EdFiExitWithdrawTypeDescriptorID = F, IsCrossEntityCourseEnrollment = F, ModifiedTime = F, SchoolYearID = F, StateStatusEndCodeMNID = F, Type = F, UserIDCreatedBy = F, UserIDModifiedBy = F, WithdrawalCodeIDClonedFrom = F, WithdrawalCodeMNID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "WithdrawalCodeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Enrollment", objectName = "WithdrawalCode", objectId = WithdrawalCodeID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a WithdrawalCode
	#'
	#' This function deletes a WithdrawalCode
	#' @param WithdrawalCodeID The ID of the WithdrawalCode to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The WithdrawalCodeID of the deleted WithdrawalCode.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteWithdrawalCode <- function(WithdrawalCodeID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Enrollment", objectName = "WithdrawalCode", objectId = WithdrawalCodeID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a WithdrawalCode
	#'
	#' This function creates a WithdrawalCode
	#' @param fieldNames The field values to give the created WithdrawalCode. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A newly created WithdrawalCode
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createWithdrawalCode <- function(Code = NULL, Description = NULL, DistrictGroupKey = NULL, DistrictID = NULL, EdFiExitWithdrawID = NULL, EdFiExitWithdrawTypeDescriptorID = NULL, IsCrossEntityCourseEnrollment = NULL, SchoolYearID = NULL, StateStatusEndCodeMNID = NULL, Type = NULL, WithdrawalCodeIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Enrollment", objectName = "WithdrawalCode", body = list(DataObject = body), searchFields = append("WithdrawalCodeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a WithdrawalCode
	#'
	#' This function modifies a WithdrawalCode
	#' @param fieldNames The field values to give the modified WithdrawalCode. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The modified WithdrawalCode
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyWithdrawalCode <- function(WithdrawalCodeID, Code = NULL, Description = NULL, DistrictGroupKey = NULL, DistrictID = NULL, EdFiExitWithdrawID = NULL, EdFiExitWithdrawTypeDescriptorID = NULL, IsCrossEntityCourseEnrollment = NULL, SchoolYearID = NULL, StateStatusEndCodeMNID = NULL, Type = NULL, WithdrawalCodeIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Enrollment", objectName = "WithdrawalCode", objectId = WithdrawalCodeID, body = list(DataObject = body), searchFields = append("WithdrawalCodeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempStudentEnrollmentRecords
	#'
	#' This function returns a dataframe or json object of TempStudentEnrollmentRecords
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentEnrollmentRecords. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentEnrollmentRecords.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentEnrollmentRecord') to get more field paths.
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
	#' @concept Enrollment
	#' @return A list of TempStudentEnrollmentRecords
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStudentEnrollmentRecords <- function(searchConditionsList = NULL, AdvisorFullName = F, CalendarCode = F, CalendarID = F, CompletedSchoolYearOverride = F, CreatedTime = F, CreateFeeManagementCustomer = F, CreateFeeManagementCustomerEntityYear = F, DisciplineOfficerFullName = F, EdFiDistrictIDResidence = F, EdFiDistrictIDTransfer = F, EdFiDistrictResidenceCodeDescription = F, EdFiSchoolIDTransfer = F, EndDate = F, EnrollIntoEntityCode = F, EnrollmentMoveable = F, EntityCode = F, EntityID = F, EntryCode = F, EntryCodeID = F, EntryComment = F, EntryWithdrawalID = F, Error = F, ErrorCount = F, ExcludeFromHonorRoll = F, ExcludeFromRank = F, ExcludeFromThirdFridaySeptemberCount = F, FailureReason = F, FeeManagementCustomerID = F, GradeLevelCode = F, GradeReferenceID = F, GradYear = F, GSAADAClaimableOverrideCode = F, GSAADAClaimableOverrideCodeDisplayName = F, HomeRCDTSOverrideCode = F, HomeRCDTSOverrideCodeDisplayName = F, HomeRCDTSOverrideID = F, HomeroomCode = F, HomeroomID = F, IncludeAsProspectiveRank = F, IsCurrentActive = F, IsDefaultEntityForEntryWithdrawal = F, IsDefaultEntityForStudentEntityYear = F, IsPermanentExit = F, IsPrivateSchoolChoiceStudent = F, IsTuitionPaidOutOfDistrict = F, ModifiedTime = F, NumericYear = F, OutgoingStudent = F, PercentEnrolled = F, ProcessEntryWithdrawal = F, PromotionStatus = F, ScheduledSectionCount = F, SchoolCode = F, SchoolID = F, SchoolYearID = F, ServingRCDTSOverrideCode = F, ServingRCDTSOverrideCodeDisplayName = F, ServingRCDTSOverrideID = F, SourceEntryWithdrawalID = F, StaffIDAdvisor = F, StaffIDDisciplineOfficer = F, StartDate = F, StateAidCategoryMNID = F, StateDistrictMNCodeName = F, StateDistrictMNID = F, StateLastAttendanceLocationCodeMNID = F, StudentCourseRequestNotMoveableCount = F, StudentCourseRequestToDeleteCount = F, StudentFullName = F, StudentID = F, StudentNumber = F, StudentTypeCode = F, StudentTypeID = F, TempStudentEnrollmentRecordID = F, TestingSchoolCode = F, TestingSchoolCodeDisplayName = F, TestingSchoolRCDTSOverrideCode = F, TestingSchoolRCDTSOverrideCodeDisplayName = F, TestingSchoolRCDTSOverrideID = F, TotalStudentCourseRequestCount = F, UserIDCreatedBy = F, UserIDModifiedBy = F, WithdrawalCode = F, WithdrawalCodeID = F, WithdrawalComment = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "TempStudentEnrollmentRecord", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempStudentEnrollmentRecord
	#'
	#' This function returns a dataframe or json object of a TempStudentEnrollmentRecord
	#' @param TempStudentEnrollmentRecordID The ID of the TempStudentEnrollmentRecord to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentEnrollmentRecord. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentEnrollmentRecord.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentEnrollmentRecord') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A dataframe or of TempStudentEnrollmentRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempStudentEnrollmentRecord <- function(TempStudentEnrollmentRecordID, AdvisorFullName = F, CalendarCode = F, CalendarID = F, CompletedSchoolYearOverride = F, CreatedTime = F, CreateFeeManagementCustomer = F, CreateFeeManagementCustomerEntityYear = F, DisciplineOfficerFullName = F, EdFiDistrictIDResidence = F, EdFiDistrictIDTransfer = F, EdFiDistrictResidenceCodeDescription = F, EdFiSchoolIDTransfer = F, EndDate = F, EnrollIntoEntityCode = F, EnrollmentMoveable = F, EntityCode = F, EntityID = F, EntryCode = F, EntryCodeID = F, EntryComment = F, EntryWithdrawalID = F, Error = F, ErrorCount = F, ExcludeFromHonorRoll = F, ExcludeFromRank = F, ExcludeFromThirdFridaySeptemberCount = F, FailureReason = F, FeeManagementCustomerID = F, GradeLevelCode = F, GradeReferenceID = F, GradYear = F, GSAADAClaimableOverrideCode = F, GSAADAClaimableOverrideCodeDisplayName = F, HomeRCDTSOverrideCode = F, HomeRCDTSOverrideCodeDisplayName = F, HomeRCDTSOverrideID = F, HomeroomCode = F, HomeroomID = F, IncludeAsProspectiveRank = F, IsCurrentActive = F, IsDefaultEntityForEntryWithdrawal = F, IsDefaultEntityForStudentEntityYear = F, IsPermanentExit = F, IsPrivateSchoolChoiceStudent = F, IsTuitionPaidOutOfDistrict = F, ModifiedTime = F, NumericYear = F, OutgoingStudent = F, PercentEnrolled = F, ProcessEntryWithdrawal = F, PromotionStatus = F, ScheduledSectionCount = F, SchoolCode = F, SchoolID = F, SchoolYearID = F, ServingRCDTSOverrideCode = F, ServingRCDTSOverrideCodeDisplayName = F, ServingRCDTSOverrideID = F, SourceEntryWithdrawalID = F, StaffIDAdvisor = F, StaffIDDisciplineOfficer = F, StartDate = F, StateAidCategoryMNID = F, StateDistrictMNCodeName = F, StateDistrictMNID = F, StateLastAttendanceLocationCodeMNID = F, StudentCourseRequestNotMoveableCount = F, StudentCourseRequestToDeleteCount = F, StudentFullName = F, StudentID = F, StudentNumber = F, StudentTypeCode = F, StudentTypeID = F, TestingSchoolCode = F, TestingSchoolCodeDisplayName = F, TestingSchoolRCDTSOverrideCode = F, TestingSchoolRCDTSOverrideCodeDisplayName = F, TestingSchoolRCDTSOverrideID = F, TotalStudentCourseRequestCount = F, UserIDCreatedBy = F, UserIDModifiedBy = F, WithdrawalCode = F, WithdrawalCodeID = F, WithdrawalComment = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempStudentEnrollmentRecordID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Enrollment", objectName = "TempStudentEnrollmentRecord", objectId = TempStudentEnrollmentRecordID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempStudentEnrollmentRecord
	#'
	#' This function deletes a TempStudentEnrollmentRecord
	#' @param TempStudentEnrollmentRecordID The ID of the TempStudentEnrollmentRecord to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The TempStudentEnrollmentRecordID of the deleted TempStudentEnrollmentRecord.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempStudentEnrollmentRecord <- function(TempStudentEnrollmentRecordID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Enrollment", objectName = "TempStudentEnrollmentRecord", objectId = TempStudentEnrollmentRecordID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempStudentEnrollmentRecord
	#'
	#' This function creates a TempStudentEnrollmentRecord
	#' @param fieldNames The field values to give the created TempStudentEnrollmentRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A newly created TempStudentEnrollmentRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempStudentEnrollmentRecord <- function(AdvisorFullName = NULL, CalendarCode = NULL, CalendarID = NULL, CompletedSchoolYearOverride = NULL, CreateFeeManagementCustomer = NULL, CreateFeeManagementCustomerEntityYear = NULL, DisciplineOfficerFullName = NULL, EdFiDistrictIDResidence = NULL, EdFiDistrictIDTransfer = NULL, EdFiDistrictResidenceCodeDescription = NULL, EdFiSchoolIDTransfer = NULL, EndDate = NULL, EnrollIntoEntityCode = NULL, EnrollmentMoveable = NULL, EntityCode = NULL, EntityID = NULL, EntryCode = NULL, EntryCodeID = NULL, EntryComment = NULL, EntryWithdrawalID = NULL, Error = NULL, ErrorCount = NULL, ExcludeFromHonorRoll = NULL, ExcludeFromRank = NULL, ExcludeFromThirdFridaySeptemberCount = NULL, FailureReason = NULL, FeeManagementCustomerID = NULL, GradeLevelCode = NULL, GradeReferenceID = NULL, GradYear = NULL, GSAADAClaimableOverrideCode = NULL, GSAADAClaimableOverrideCodeDisplayName = NULL, HomeRCDTSOverrideCode = NULL, HomeRCDTSOverrideCodeDisplayName = NULL, HomeRCDTSOverrideID = NULL, HomeroomCode = NULL, HomeroomID = NULL, IncludeAsProspectiveRank = NULL, IsCurrentActive = NULL, IsDefaultEntityForEntryWithdrawal = NULL, IsDefaultEntityForStudentEntityYear = NULL, IsPermanentExit = NULL, IsPrivateSchoolChoiceStudent = NULL, IsTuitionPaidOutOfDistrict = NULL, NumericYear = NULL, OutgoingStudent = NULL, PercentEnrolled = NULL, ProcessEntryWithdrawal = NULL, PromotionStatus = NULL, ScheduledSectionCount = NULL, SchoolCode = NULL, SchoolID = NULL, SchoolYearID = NULL, ServingRCDTSOverrideCode = NULL, ServingRCDTSOverrideCodeDisplayName = NULL, ServingRCDTSOverrideID = NULL, SourceEntryWithdrawalID = NULL, StaffIDAdvisor = NULL, StaffIDDisciplineOfficer = NULL, StartDate = NULL, StateAidCategoryMNID = NULL, StateDistrictMNCodeName = NULL, StateDistrictMNID = NULL, StateLastAttendanceLocationCodeMNID = NULL, StudentCourseRequestNotMoveableCount = NULL, StudentCourseRequestToDeleteCount = NULL, StudentFullName = NULL, StudentID = NULL, StudentNumber = NULL, StudentTypeCode = NULL, StudentTypeID = NULL, TestingSchoolCode = NULL, TestingSchoolCodeDisplayName = NULL, TestingSchoolRCDTSOverrideCode = NULL, TestingSchoolRCDTSOverrideCodeDisplayName = NULL, TestingSchoolRCDTSOverrideID = NULL, TotalStudentCourseRequestCount = NULL, WithdrawalCode = NULL, WithdrawalCodeID = NULL, WithdrawalComment = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Enrollment", objectName = "TempStudentEnrollmentRecord", body = list(DataObject = body), searchFields = append("TempStudentEnrollmentRecordID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempStudentEnrollmentRecord
	#'
	#' This function modifies a TempStudentEnrollmentRecord
	#' @param fieldNames The field values to give the modified TempStudentEnrollmentRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The modified TempStudentEnrollmentRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempStudentEnrollmentRecord <- function(TempStudentEnrollmentRecordID, AdvisorFullName = NULL, CalendarCode = NULL, CalendarID = NULL, CompletedSchoolYearOverride = NULL, CreateFeeManagementCustomer = NULL, CreateFeeManagementCustomerEntityYear = NULL, DisciplineOfficerFullName = NULL, EdFiDistrictIDResidence = NULL, EdFiDistrictIDTransfer = NULL, EdFiDistrictResidenceCodeDescription = NULL, EdFiSchoolIDTransfer = NULL, EndDate = NULL, EnrollIntoEntityCode = NULL, EnrollmentMoveable = NULL, EntityCode = NULL, EntityID = NULL, EntryCode = NULL, EntryCodeID = NULL, EntryComment = NULL, EntryWithdrawalID = NULL, Error = NULL, ErrorCount = NULL, ExcludeFromHonorRoll = NULL, ExcludeFromRank = NULL, ExcludeFromThirdFridaySeptemberCount = NULL, FailureReason = NULL, FeeManagementCustomerID = NULL, GradeLevelCode = NULL, GradeReferenceID = NULL, GradYear = NULL, GSAADAClaimableOverrideCode = NULL, GSAADAClaimableOverrideCodeDisplayName = NULL, HomeRCDTSOverrideCode = NULL, HomeRCDTSOverrideCodeDisplayName = NULL, HomeRCDTSOverrideID = NULL, HomeroomCode = NULL, HomeroomID = NULL, IncludeAsProspectiveRank = NULL, IsCurrentActive = NULL, IsDefaultEntityForEntryWithdrawal = NULL, IsDefaultEntityForStudentEntityYear = NULL, IsPermanentExit = NULL, IsPrivateSchoolChoiceStudent = NULL, IsTuitionPaidOutOfDistrict = NULL, NumericYear = NULL, OutgoingStudent = NULL, PercentEnrolled = NULL, ProcessEntryWithdrawal = NULL, PromotionStatus = NULL, ScheduledSectionCount = NULL, SchoolCode = NULL, SchoolID = NULL, SchoolYearID = NULL, ServingRCDTSOverrideCode = NULL, ServingRCDTSOverrideCodeDisplayName = NULL, ServingRCDTSOverrideID = NULL, SourceEntryWithdrawalID = NULL, StaffIDAdvisor = NULL, StaffIDDisciplineOfficer = NULL, StartDate = NULL, StateAidCategoryMNID = NULL, StateDistrictMNCodeName = NULL, StateDistrictMNID = NULL, StateLastAttendanceLocationCodeMNID = NULL, StudentCourseRequestNotMoveableCount = NULL, StudentCourseRequestToDeleteCount = NULL, StudentFullName = NULL, StudentID = NULL, StudentNumber = NULL, StudentTypeCode = NULL, StudentTypeID = NULL, TestingSchoolCode = NULL, TestingSchoolCodeDisplayName = NULL, TestingSchoolRCDTSOverrideCode = NULL, TestingSchoolRCDTSOverrideCodeDisplayName = NULL, TestingSchoolRCDTSOverrideID = NULL, TotalStudentCourseRequestCount = NULL, WithdrawalCode = NULL, WithdrawalCodeID = NULL, WithdrawalComment = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Enrollment", objectName = "TempStudentEnrollmentRecord", objectId = TempStudentEnrollmentRecordID, body = list(DataObject = body), searchFields = append("TempStudentEnrollmentRecordID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempAffectedWithdrawalRecords
	#'
	#' This function returns a dataframe or json object of TempAffectedWithdrawalRecords
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAffectedWithdrawalRecords. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAffectedWithdrawalRecords.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAffectedWithdrawalRecord') to get more field paths.
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
	#' @concept Enrollment
	#' @return A list of TempAffectedWithdrawalRecords
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempAffectedWithdrawalRecords <- function(searchConditionsList = NULL, Action = F, ActionMessage = F, AffectedPrimaryKey = F, CourseID = F, CreatedTime = F, Description = F, EarlyExitReason = F, EarlyExitReasonID = F, EndDate = F, EntityID = F, HasAttendance = F, HasFutureAttendance = F, HasFutureGrades = F, HasGrades = F, HasPartiallyPaidFees = F, HasTransactionPreventingStudentSectionDelete = F, IsFutureEntryWithdrawal = F, IsNoShowDeleteNoOtherEnrollmentInTheDistrict = F, ModifiedTime = F, MostFutureGradeStartDate = F, NameIDRequestedBy = F, NewEndDate = F, ParentPrimaryKey = F, RecordType = F, SchoolYearID = F, Section = F, SectionID = F, StartDate = F, StudentID = F, TempAffectedWithdrawalRecordID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "TempAffectedWithdrawalRecord", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempAffectedWithdrawalRecord
	#'
	#' This function returns a dataframe or json object of a TempAffectedWithdrawalRecord
	#' @param TempAffectedWithdrawalRecordID The ID of the TempAffectedWithdrawalRecord to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAffectedWithdrawalRecord. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAffectedWithdrawalRecord.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAffectedWithdrawalRecord') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A dataframe or of TempAffectedWithdrawalRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempAffectedWithdrawalRecord <- function(TempAffectedWithdrawalRecordID, Action = F, ActionMessage = F, AffectedPrimaryKey = F, CourseID = F, CreatedTime = F, Description = F, EarlyExitReason = F, EarlyExitReasonID = F, EndDate = F, EntityID = F, HasAttendance = F, HasFutureAttendance = F, HasFutureGrades = F, HasGrades = F, HasPartiallyPaidFees = F, HasTransactionPreventingStudentSectionDelete = F, IsFutureEntryWithdrawal = F, IsNoShowDeleteNoOtherEnrollmentInTheDistrict = F, ModifiedTime = F, MostFutureGradeStartDate = F, NameIDRequestedBy = F, NewEndDate = F, ParentPrimaryKey = F, RecordType = F, SchoolYearID = F, Section = F, SectionID = F, StartDate = F, StudentID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempAffectedWithdrawalRecordID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Enrollment", objectName = "TempAffectedWithdrawalRecord", objectId = TempAffectedWithdrawalRecordID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempAffectedWithdrawalRecord
	#'
	#' This function deletes a TempAffectedWithdrawalRecord
	#' @param TempAffectedWithdrawalRecordID The ID of the TempAffectedWithdrawalRecord to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The TempAffectedWithdrawalRecordID of the deleted TempAffectedWithdrawalRecord.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempAffectedWithdrawalRecord <- function(TempAffectedWithdrawalRecordID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Enrollment", objectName = "TempAffectedWithdrawalRecord", objectId = TempAffectedWithdrawalRecordID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempAffectedWithdrawalRecord
	#'
	#' This function creates a TempAffectedWithdrawalRecord
	#' @param fieldNames The field values to give the created TempAffectedWithdrawalRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A newly created TempAffectedWithdrawalRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempAffectedWithdrawalRecord <- function(Action = NULL, ActionMessage = NULL, AffectedPrimaryKey = NULL, CourseID = NULL, Description = NULL, EarlyExitReason = NULL, EarlyExitReasonID = NULL, EndDate = NULL, EntityID = NULL, HasAttendance = NULL, HasFutureAttendance = NULL, HasFutureGrades = NULL, HasGrades = NULL, HasPartiallyPaidFees = NULL, HasTransactionPreventingStudentSectionDelete = NULL, IsFutureEntryWithdrawal = NULL, IsNoShowDeleteNoOtherEnrollmentInTheDistrict = NULL, MostFutureGradeStartDate = NULL, NameIDRequestedBy = NULL, NewEndDate = NULL, ParentPrimaryKey = NULL, RecordType = NULL, SchoolYearID = NULL, Section = NULL, SectionID = NULL, StartDate = NULL, StudentID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Enrollment", objectName = "TempAffectedWithdrawalRecord", body = list(DataObject = body), searchFields = append("TempAffectedWithdrawalRecordID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempAffectedWithdrawalRecord
	#'
	#' This function modifies a TempAffectedWithdrawalRecord
	#' @param fieldNames The field values to give the modified TempAffectedWithdrawalRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The modified TempAffectedWithdrawalRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempAffectedWithdrawalRecord <- function(TempAffectedWithdrawalRecordID, Action = NULL, ActionMessage = NULL, AffectedPrimaryKey = NULL, CourseID = NULL, Description = NULL, EarlyExitReason = NULL, EarlyExitReasonID = NULL, EndDate = NULL, EntityID = NULL, HasAttendance = NULL, HasFutureAttendance = NULL, HasFutureGrades = NULL, HasGrades = NULL, HasPartiallyPaidFees = NULL, HasTransactionPreventingStudentSectionDelete = NULL, IsFutureEntryWithdrawal = NULL, IsNoShowDeleteNoOtherEnrollmentInTheDistrict = NULL, MostFutureGradeStartDate = NULL, NameIDRequestedBy = NULL, NewEndDate = NULL, ParentPrimaryKey = NULL, RecordType = NULL, SchoolYearID = NULL, Section = NULL, SectionID = NULL, StartDate = NULL, StudentID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Enrollment", objectName = "TempAffectedWithdrawalRecord", objectId = TempAffectedWithdrawalRecordID, body = list(DataObject = body), searchFields = append("TempAffectedWithdrawalRecordID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EnrollmentConfigDistricts
	#'
	#' This function returns a dataframe or json object of EnrollmentConfigDistricts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EnrollmentConfigDistricts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EnrollmentConfigDistricts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EnrollmentConfigDistrict') to get more field paths.
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
	#' @concept Enrollment
	#' @return A list of EnrollmentConfigDistricts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEnrollmentConfigDistricts <- function(searchConditionsList = NULL, AllowDualEnrollment = F, ConfigDistrictID = F, CreatedTime = F, DistrictID = F, EntryDaysBeforeCalendarStart = F, ModifiedTime = F, NumberDaysBackdateEntry = F, NumberDaysBackdateWithdrawal = F, UserIDCreatedBy = F, UserIDModifiedBy = F, WithdrawalDaysAfterCalendarEnd = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "ConfigDistrict", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EnrollmentConfigDistrict
	#'
	#' This function returns a dataframe or json object of an EnrollmentConfigDistrict
	#' @param EnrollmentConfigDistrictID The ID of the EnrollmentConfigDistrict to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EnrollmentConfigDistrict. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EnrollmentConfigDistrict.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EnrollmentConfigDistrict') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A dataframe or of EnrollmentConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEnrollmentConfigDistrict <- function(EnrollmentConfigDistrictID, AllowDualEnrollment = F, ConfigDistrictID = F, CreatedTime = F, DistrictID = F, EntryDaysBeforeCalendarStart = F, ModifiedTime = F, NumberDaysBackdateEntry = F, NumberDaysBackdateWithdrawal = F, UserIDCreatedBy = F, UserIDModifiedBy = F, WithdrawalDaysAfterCalendarEnd = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EnrollmentConfigDistrictID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Enrollment", objectName = "ConfigDistrict", objectId = EnrollmentConfigDistrictID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EnrollmentConfigDistrict
	#'
	#' This function deletes an EnrollmentConfigDistrict
	#' @param EnrollmentConfigDistrictID The ID of the EnrollmentConfigDistrict to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The EnrollmentConfigDistrictID of the deleted EnrollmentConfigDistrict.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEnrollmentConfigDistrict <- function(EnrollmentConfigDistrictID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Enrollment", objectName = "ConfigDistrict", objectId = EnrollmentConfigDistrictID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EnrollmentConfigDistrict
	#'
	#' This function creates an EnrollmentConfigDistrict
	#' @param fieldNames The field values to give the created EnrollmentConfigDistrict. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A newly created EnrollmentConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEnrollmentConfigDistrict <- function(AllowDualEnrollment = NULL, DistrictID = NULL, EntryDaysBeforeCalendarStart = NULL, NumberDaysBackdateEntry = NULL, NumberDaysBackdateWithdrawal = NULL, WithdrawalDaysAfterCalendarEnd = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Enrollment", objectName = "ConfigDistrict", body = list(DataObject = body), searchFields = append("ConfigDistrictID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EnrollmentConfigDistrict
	#'
	#' This function modifies an EnrollmentConfigDistrict
	#' @param fieldNames The field values to give the modified EnrollmentConfigDistrict. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The modified EnrollmentConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEnrollmentConfigDistrict <- function(ConfigDistrictID, AllowDualEnrollment = NULL, DistrictID = NULL, EntryDaysBeforeCalendarStart = NULL, NumberDaysBackdateEntry = NULL, NumberDaysBackdateWithdrawal = NULL, WithdrawalDaysAfterCalendarEnd = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Enrollment", objectName = "ConfigDistrict", objectId = ConfigDistrictID, body = list(DataObject = body), searchFields = append("ConfigDistrictID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EntitySchools
	#'
	#' This function returns a dataframe or json object of EntitySchools
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EntitySchools. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EntitySchools.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EntitySchool') to get more field paths.
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
	#' @concept Enrollment
	#' @return A list of EntitySchools
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEntitySchools <- function(searchConditionsList = NULL, CreatedTime = F, EntityID = F, EntitySchoolID = F, EntitySchoolIDClonedFrom = F, EntitySchoolIDClonedTo = F, IsDefaultEntityForSchool = F, IsDefaultSchoolForEntity = F, IsOnlySchoolInEntity = F, ModifiedTime = F, SchoolID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "EntitySchool", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EntitySchool
	#'
	#' This function returns a dataframe or json object of an EntitySchool
	#' @param EntitySchoolID The ID of the EntitySchool to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EntitySchool. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EntitySchool.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EntitySchool') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A dataframe or of EntitySchool
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEntitySchool <- function(EntitySchoolID, CreatedTime = F, EntityID = F, EntitySchoolIDClonedFrom = F, EntitySchoolIDClonedTo = F, IsDefaultEntityForSchool = F, IsDefaultSchoolForEntity = F, IsOnlySchoolInEntity = F, ModifiedTime = F, SchoolID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EntitySchoolID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Enrollment", objectName = "EntitySchool", objectId = EntitySchoolID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EntitySchool
	#'
	#' This function deletes an EntitySchool
	#' @param EntitySchoolID The ID of the EntitySchool to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The EntitySchoolID of the deleted EntitySchool.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEntitySchool <- function(EntitySchoolID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Enrollment", objectName = "EntitySchool", objectId = EntitySchoolID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EntitySchool
	#'
	#' This function creates an EntitySchool
	#' @param fieldNames The field values to give the created EntitySchool. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A newly created EntitySchool
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEntitySchool <- function(EntityID = NULL, EntitySchoolIDClonedFrom = NULL, IsDefaultEntityForSchool = NULL, IsDefaultSchoolForEntity = NULL, SchoolID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Enrollment", objectName = "EntitySchool", body = list(DataObject = body), searchFields = append("EntitySchoolID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EntitySchool
	#'
	#' This function modifies an EntitySchool
	#' @param fieldNames The field values to give the modified EntitySchool. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The modified EntitySchool
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEntitySchool <- function(EntitySchoolID, EntityID = NULL, EntitySchoolIDClonedFrom = NULL, IsDefaultEntityForSchool = NULL, IsDefaultSchoolForEntity = NULL, SchoolID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Enrollment", objectName = "EntitySchool", objectId = EntitySchoolID, body = list(DataObject = body), searchFields = append("EntitySchoolID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EntryCodes
	#'
	#' This function returns a dataframe or json object of EntryCodes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EntryCodes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EntryCodes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EntryCode') to get more field paths.
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
	#' @concept Enrollment
	#' @return A list of EntryCodes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEntryCodes <- function(searchConditionsList = NULL, Code = F, CodeDescription = F, CreatedTime = F, Description = F, DistrictGroupKey = F, DistrictID = F, EdFiEntryTypeDescriptorID = F, EntryCodeID = F, EntryCodeIDClonedFrom = F, IsCrossEntityCourseEnrollment = F, ModifiedTime = F, SchoolYearID = F, Type = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "EntryCode", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EntryCode
	#'
	#' This function returns a dataframe or json object of an EntryCode
	#' @param EntryCodeID The ID of the EntryCode to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EntryCode. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EntryCode.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EntryCode') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A dataframe or of EntryCode
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEntryCode <- function(EntryCodeID, Code = F, CodeDescription = F, CreatedTime = F, Description = F, DistrictGroupKey = F, DistrictID = F, EdFiEntryTypeDescriptorID = F, EntryCodeIDClonedFrom = F, IsCrossEntityCourseEnrollment = F, ModifiedTime = F, SchoolYearID = F, Type = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EntryCodeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Enrollment", objectName = "EntryCode", objectId = EntryCodeID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EntryCode
	#'
	#' This function deletes an EntryCode
	#' @param EntryCodeID The ID of the EntryCode to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The EntryCodeID of the deleted EntryCode.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEntryCode <- function(EntryCodeID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Enrollment", objectName = "EntryCode", objectId = EntryCodeID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EntryCode
	#'
	#' This function creates an EntryCode
	#' @param fieldNames The field values to give the created EntryCode. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A newly created EntryCode
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEntryCode <- function(Code = NULL, Description = NULL, DistrictGroupKey = NULL, DistrictID = NULL, EdFiEntryTypeDescriptorID = NULL, EntryCodeIDClonedFrom = NULL, IsCrossEntityCourseEnrollment = NULL, SchoolYearID = NULL, Type = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Enrollment", objectName = "EntryCode", body = list(DataObject = body), searchFields = append("EntryCodeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EntryCode
	#'
	#' This function modifies an EntryCode
	#' @param fieldNames The field values to give the modified EntryCode. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The modified EntryCode
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEntryCode <- function(EntryCodeID, Code = NULL, Description = NULL, DistrictGroupKey = NULL, DistrictID = NULL, EdFiEntryTypeDescriptorID = NULL, EntryCodeIDClonedFrom = NULL, IsCrossEntityCourseEnrollment = NULL, SchoolYearID = NULL, Type = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Enrollment", objectName = "EntryCode", objectId = EntryCodeID, body = list(DataObject = body), searchFields = append("EntryCodeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List GradeLevels
	#'
	#' This function returns a dataframe or json object of GradeLevels
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given GradeLevels. Defaults to FALSE for all return fields which, for convenience, returns all fields for the GradeLevels.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('GradeLevel') to get more field paths.
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
	#' @concept Enrollment
	#' @return A list of GradeLevels
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listGradeLevels <- function(searchConditionsList = NULL, Code = F, CodeDescription = F, CommonEducationDataStandardsGradeLevelID = F, CreatedTime = F, Description = F, DistrictGroupKey = F, DistrictID = F, EdFiGradeLevelDescriptorID = F, FederalGradeLevel = F, GradeLevelID = F, IlluminateOverride = F, ModifiedTime = F, NumericValue = F, StateGradeLevel = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "GradeLevel", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a GradeLevel
	#'
	#' This function returns a dataframe or json object of a GradeLevel
	#' @param GradeLevelID The ID of the GradeLevel to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given GradeLevel. Defaults to FALSE for all return fields which, for convenience, returns all fields for the GradeLevel.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('GradeLevel') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A dataframe or of GradeLevel
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getGradeLevel <- function(GradeLevelID, Code = F, CodeDescription = F, CommonEducationDataStandardsGradeLevelID = F, CreatedTime = F, Description = F, DistrictGroupKey = F, DistrictID = F, EdFiGradeLevelDescriptorID = F, FederalGradeLevel = F, IlluminateOverride = F, ModifiedTime = F, NumericValue = F, StateGradeLevel = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "GradeLevelID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Enrollment", objectName = "GradeLevel", objectId = GradeLevelID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a GradeLevel
	#'
	#' This function deletes a GradeLevel
	#' @param GradeLevelID The ID of the GradeLevel to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The GradeLevelID of the deleted GradeLevel.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteGradeLevel <- function(GradeLevelID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Enrollment", objectName = "GradeLevel", objectId = GradeLevelID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a GradeLevel
	#'
	#' This function creates a GradeLevel
	#' @param fieldNames The field values to give the created GradeLevel. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A newly created GradeLevel
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createGradeLevel <- function(Code = NULL, CommonEducationDataStandardsGradeLevelID = NULL, Description = NULL, DistrictGroupKey = NULL, DistrictID = NULL, EdFiGradeLevelDescriptorID = NULL, FederalGradeLevel = NULL, IlluminateOverride = NULL, NumericValue = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Enrollment", objectName = "GradeLevel", body = list(DataObject = body), searchFields = append("GradeLevelID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a GradeLevel
	#'
	#' This function modifies a GradeLevel
	#' @param fieldNames The field values to give the modified GradeLevel. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The modified GradeLevel
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyGradeLevel <- function(GradeLevelID, Code = NULL, CommonEducationDataStandardsGradeLevelID = NULL, Description = NULL, DistrictGroupKey = NULL, DistrictID = NULL, EdFiGradeLevelDescriptorID = NULL, FederalGradeLevel = NULL, IlluminateOverride = NULL, NumericValue = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Enrollment", objectName = "GradeLevel", objectId = GradeLevelID, body = list(DataObject = body), searchFields = append("GradeLevelID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List GradeReferences
	#'
	#' This function returns a dataframe or json object of GradeReferences
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given GradeReferences. Defaults to FALSE for all return fields which, for convenience, returns all fields for the GradeReferences.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('GradeReference') to get more field paths.
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
	#' @concept Enrollment
	#' @return A list of GradeReferences
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listGradeReferences <- function(searchConditionsList = NULL, CreatedTime = F, DistrictGroupKey = F, GradeLevelCode = F, GradeLevelID = F, GradeReferenceID = F, GradeReferenceIDClonedFrom = F, GradeReferenceIDClonedTo = F, GradeReferenceMNID = F, GradYear = F, MinutesPresentFullDay = F, MinutesPresentHalfDay = F, ModifiedTime = F, SchoolYearID = F, StateGradeLevel = F, StateSTARGradeLevelMNID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "GradeReference", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a GradeReference
	#'
	#' This function returns a dataframe or json object of a GradeReference
	#' @param GradeReferenceID The ID of the GradeReference to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given GradeReference. Defaults to FALSE for all return fields which, for convenience, returns all fields for the GradeReference.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('GradeReference') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A dataframe or of GradeReference
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getGradeReference <- function(GradeReferenceID, CreatedTime = F, DistrictGroupKey = F, GradeLevelCode = F, GradeLevelID = F, GradeReferenceIDClonedFrom = F, GradeReferenceIDClonedTo = F, GradeReferenceMNID = F, GradYear = F, MinutesPresentFullDay = F, MinutesPresentHalfDay = F, ModifiedTime = F, SchoolYearID = F, StateGradeLevel = F, StateSTARGradeLevelMNID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "GradeReferenceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Enrollment", objectName = "GradeReference", objectId = GradeReferenceID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a GradeReference
	#'
	#' This function deletes a GradeReference
	#' @param GradeReferenceID The ID of the GradeReference to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The GradeReferenceID of the deleted GradeReference.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteGradeReference <- function(GradeReferenceID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Enrollment", objectName = "GradeReference", objectId = GradeReferenceID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a GradeReference
	#'
	#' This function creates a GradeReference
	#' @param fieldNames The field values to give the created GradeReference. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A newly created GradeReference
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createGradeReference <- function(DistrictGroupKey = NULL, GradeLevelID = NULL, GradeReferenceIDClonedFrom = NULL, GradYear = NULL, MinutesPresentFullDay = NULL, MinutesPresentHalfDay = NULL, SchoolYearID = NULL, StateSTARGradeLevelMNID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Enrollment", objectName = "GradeReference", body = list(DataObject = body), searchFields = append("GradeReferenceID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a GradeReference
	#'
	#' This function modifies a GradeReference
	#' @param fieldNames The field values to give the modified GradeReference. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The modified GradeReference
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyGradeReference <- function(GradeReferenceID, DistrictGroupKey = NULL, GradeLevelID = NULL, GradeReferenceIDClonedFrom = NULL, GradYear = NULL, MinutesPresentFullDay = NULL, MinutesPresentHalfDay = NULL, SchoolYearID = NULL, StateSTARGradeLevelMNID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Enrollment", objectName = "GradeReference", objectId = GradeReferenceID, body = list(DataObject = body), searchFields = append("GradeReferenceID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Homerooms
	#'
	#' This function returns a dataframe or json object of Homerooms
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Homerooms. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Homerooms.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Homeroom') to get more field paths.
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
	#' @concept Enrollment
	#' @return A list of Homerooms
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listHomerooms <- function(searchConditionsList = NULL, Code = F, CreatedTime = F, EntityID = F, HomeroomDetails = F, HomeroomID = F, HomeroomIDClonedFrom = F, HomeroomIDClonedTo = F, ModifiedTime = F, RoomID = F, SchoolYearID = F, StaffID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "Homeroom", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Homeroom
	#'
	#' This function returns a dataframe or json object of a Homeroom
	#' @param HomeroomID The ID of the Homeroom to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Homeroom. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Homeroom.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Homeroom') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A dataframe or of Homeroom
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getHomeroom <- function(HomeroomID, Code = F, CreatedTime = F, EntityID = F, HomeroomDetails = F, HomeroomIDClonedFrom = F, HomeroomIDClonedTo = F, ModifiedTime = F, RoomID = F, SchoolYearID = F, StaffID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "HomeroomID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Enrollment", objectName = "Homeroom", objectId = HomeroomID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Homeroom
	#'
	#' This function deletes a Homeroom
	#' @param HomeroomID The ID of the Homeroom to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The HomeroomID of the deleted Homeroom.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteHomeroom <- function(HomeroomID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Enrollment", objectName = "Homeroom", objectId = HomeroomID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Homeroom
	#'
	#' This function creates a Homeroom
	#' @param fieldNames The field values to give the created Homeroom. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A newly created Homeroom
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createHomeroom <- function(Code = NULL, EntityID = NULL, HomeroomIDClonedFrom = NULL, RoomID = NULL, SchoolYearID = NULL, StaffID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Enrollment", objectName = "Homeroom", body = list(DataObject = body), searchFields = append("HomeroomID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Homeroom
	#'
	#' This function modifies a Homeroom
	#' @param fieldNames The field values to give the modified Homeroom. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The modified Homeroom
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyHomeroom <- function(HomeroomID, Code = NULL, EntityID = NULL, HomeroomIDClonedFrom = NULL, RoomID = NULL, SchoolYearID = NULL, StaffID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Enrollment", objectName = "Homeroom", objectId = HomeroomID, body = list(DataObject = body), searchFields = append("HomeroomID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentEntityYears
	#'
	#' This function returns a dataframe or json object of StudentEntityYears
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentEntityYears. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentEntityYears.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentEntityYear') to get more field paths.
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
	#' @concept Enrollment
	#' @return A list of StudentEntityYears
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentEntityYears <- function(searchConditionsList = NULL, ChromebookDocumentsReturned = F, CreatedTime = F, CurrentPercentEnrolled = F, DaysAbsentYTD = F, DaysEnrolledYTD = F, DaysExcusedYTD = F, DaysOtherYTD = F, DaysUnexcusedYTD = F, EntityID = F, EntryWithdrawalIDLatest = F, ExcludeFromHonorRoll = F, ExcludeFromRank = F, ExistsConflictedStudentCourseRequests = F, ExistsUnscheduleableStudentSections = F, FeeAmountDue = F, FeeChargeAmount = F, FeePaidAmount = F, FeePaidAndWaivedAmount = F, FeeUnappliedAmount = F, FeeWaivedAmount = F, FirstName = F, FlaggedMissingAssignmentsCount = F, Grade = F, HandbookSigned = F, HasActiveCareerPlanDeclarationTimePeriod = F, HasActiveEndorsementDeclarationTimePeriod = F, HasConflictedStudentCourseRequest = F, HasFlaggedMissingAssignments = F, HasMissingAssignments = F, HasNoAttendanceToday = F, HasNonCrossEntityCourseSchedulingEntryWithdrawal = F, HasOpenDisplayPeriodsInRegularSchoolDay = F, HasOverscheduledPeriod = F, HasValidStudentPlan = F, HomeroomCodeFollettDestiny = F, HomeroomID = F, HomeroomPeriodFollettDestiny = F, HomeroomStaffNameFollettDestiny = F, IncludeAsProspectiveRank = F, IsActive = F, IsCrossEntityCourseEnrollment = F, IsDefaultEntity = F, IsTransportationRequested = F, LastName = F, MiddleName = F, ModifiedTime = F, NameID = F, NumberOfStudentCourseRequests = F, NumberOfStudentSections = F, OptOutOfMedia = F, SchedulingCategories = F, SchedulingTeamID = F, SchoolIDPathExpectedSchool = F, SchoolYearID = F, SectionLengthAbsent = F, SectionLengthEnrolled = F, Semester2Absent = F, Semester2Enrolled = F, SignedAcceptableUsePolicy = F, StaffIDAdvisor = F, StaffIDDisciplineOfficer = F, StudentEntityYearID = F, StudentID = F, StudentNumber = F, TardyCountYTD = F, TardyKioskTotals = F, TotalEarnedCreditsPossibleAnticipatedNonTransferStudentSectionsNonAlternateRequestCredits = F, TotalMissedAssignmentCount = F, UILFeeReceived = F, UnscheduleableStudentSectionCount = F, UnscheduledStudentCourseRequestCount = F, UserIDCreatedBy = F, UserIDModifiedBy = F, WithdrawalDate = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "StudentEntityYear", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentEntityYear
	#'
	#' This function returns a dataframe or json object of a StudentEntityYear
	#' @param StudentEntityYearID The ID of the StudentEntityYear to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentEntityYear. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentEntityYear.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentEntityYear') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A dataframe or of StudentEntityYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentEntityYear <- function(StudentEntityYearID, ChromebookDocumentsReturned = F, CreatedTime = F, CurrentPercentEnrolled = F, DaysAbsentYTD = F, DaysEnrolledYTD = F, DaysExcusedYTD = F, DaysOtherYTD = F, DaysUnexcusedYTD = F, EntityID = F, EntryWithdrawalIDLatest = F, ExcludeFromHonorRoll = F, ExcludeFromRank = F, ExistsConflictedStudentCourseRequests = F, ExistsUnscheduleableStudentSections = F, FeeAmountDue = F, FeeChargeAmount = F, FeePaidAmount = F, FeePaidAndWaivedAmount = F, FeeUnappliedAmount = F, FeeWaivedAmount = F, FirstName = F, FlaggedMissingAssignmentsCount = F, Grade = F, HandbookSigned = F, HasActiveCareerPlanDeclarationTimePeriod = F, HasActiveEndorsementDeclarationTimePeriod = F, HasConflictedStudentCourseRequest = F, HasFlaggedMissingAssignments = F, HasMissingAssignments = F, HasNoAttendanceToday = F, HasNonCrossEntityCourseSchedulingEntryWithdrawal = F, HasOpenDisplayPeriodsInRegularSchoolDay = F, HasOverscheduledPeriod = F, HasValidStudentPlan = F, HomeroomCodeFollettDestiny = F, HomeroomID = F, HomeroomPeriodFollettDestiny = F, HomeroomStaffNameFollettDestiny = F, IncludeAsProspectiveRank = F, IsActive = F, IsCrossEntityCourseEnrollment = F, IsDefaultEntity = F, IsTransportationRequested = F, LastName = F, MiddleName = F, ModifiedTime = F, NameID = F, NumberOfStudentCourseRequests = F, NumberOfStudentSections = F, OptOutOfMedia = F, SchedulingCategories = F, SchedulingTeamID = F, SchoolIDPathExpectedSchool = F, SchoolYearID = F, SectionLengthAbsent = F, SectionLengthEnrolled = F, Semester2Absent = F, Semester2Enrolled = F, SignedAcceptableUsePolicy = F, StaffIDAdvisor = F, StaffIDDisciplineOfficer = F, StudentID = F, StudentNumber = F, TardyCountYTD = F, TardyKioskTotals = F, TotalEarnedCreditsPossibleAnticipatedNonTransferStudentSectionsNonAlternateRequestCredits = F, TotalMissedAssignmentCount = F, UILFeeReceived = F, UnscheduleableStudentSectionCount = F, UnscheduledStudentCourseRequestCount = F, UserIDCreatedBy = F, UserIDModifiedBy = F, WithdrawalDate = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentEntityYearID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Enrollment", objectName = "StudentEntityYear", objectId = StudentEntityYearID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentEntityYear
	#'
	#' This function deletes a StudentEntityYear
	#' @param StudentEntityYearID The ID of the StudentEntityYear to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The StudentEntityYearID of the deleted StudentEntityYear.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentEntityYear <- function(StudentEntityYearID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Enrollment", objectName = "StudentEntityYear", objectId = StudentEntityYearID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentEntityYear
	#'
	#' This function creates a StudentEntityYear
	#' @param fieldNames The field values to give the created StudentEntityYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A newly created StudentEntityYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentEntityYear <- function(ChromebookDocumentsReturned = NULL, CurrentPercentEnrolled = NULL, EntityID = NULL, EntryWithdrawalIDLatest = NULL, ExcludeFromHonorRoll = NULL, ExcludeFromRank = NULL, Grade = NULL, HandbookSigned = NULL, HomeroomID = NULL, IncludeAsProspectiveRank = NULL, IsActive = NULL, IsCrossEntityCourseEnrollment = NULL, IsDefaultEntity = NULL, IsTransportationRequested = NULL, NameID = NULL, OptOutOfMedia = NULL, SchedulingTeamID = NULL, SchoolIDPathExpectedSchool = NULL, SchoolYearID = NULL, SignedAcceptableUsePolicy = NULL, StaffIDAdvisor = NULL, StaffIDDisciplineOfficer = NULL, StudentID = NULL, StudentNumber = NULL, UILFeeReceived = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Enrollment", objectName = "StudentEntityYear", body = list(DataObject = body), searchFields = append("StudentEntityYearID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentEntityYear
	#'
	#' This function modifies a StudentEntityYear
	#' @param fieldNames The field values to give the modified StudentEntityYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The modified StudentEntityYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentEntityYear <- function(StudentEntityYearID, ChromebookDocumentsReturned = NULL, CurrentPercentEnrolled = NULL, EntityID = NULL, EntryWithdrawalIDLatest = NULL, ExcludeFromHonorRoll = NULL, ExcludeFromRank = NULL, Grade = NULL, HandbookSigned = NULL, HomeroomID = NULL, IncludeAsProspectiveRank = NULL, IsActive = NULL, IsCrossEntityCourseEnrollment = NULL, IsDefaultEntity = NULL, IsTransportationRequested = NULL, NameID = NULL, OptOutOfMedia = NULL, SchedulingTeamID = NULL, SchoolIDPathExpectedSchool = NULL, SchoolYearID = NULL, SignedAcceptableUsePolicy = NULL, StaffIDAdvisor = NULL, StaffIDDisciplineOfficer = NULL, StudentID = NULL, StudentNumber = NULL, UILFeeReceived = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Enrollment", objectName = "StudentEntityYear", objectId = StudentEntityYearID, body = list(DataObject = body), searchFields = append("StudentEntityYearID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentTypes
	#'
	#' This function returns a dataframe or json object of StudentTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentType') to get more field paths.
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
	#' @concept Enrollment
	#' @return A list of StudentTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentTypes <- function(searchConditionsList = NULL, Code = F, CodeDescription = F, CreatedTime = F, Description = F, DistrictGroupKey = F, DistrictID = F, ModifiedTime = F, StudentTypeID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "StudentType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentType
	#'
	#' This function returns a dataframe or json object of a StudentType
	#' @param StudentTypeID The ID of the StudentType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A dataframe or of StudentType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentType <- function(StudentTypeID, Code = F, CodeDescription = F, CreatedTime = F, Description = F, DistrictGroupKey = F, DistrictID = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Enrollment", objectName = "StudentType", objectId = StudentTypeID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentType
	#'
	#' This function deletes a StudentType
	#' @param StudentTypeID The ID of the StudentType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The StudentTypeID of the deleted StudentType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentType <- function(StudentTypeID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Enrollment", objectName = "StudentType", objectId = StudentTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentType
	#'
	#' This function creates a StudentType
	#' @param fieldNames The field values to give the created StudentType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A newly created StudentType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentType <- function(Code = NULL, Description = NULL, DistrictGroupKey = NULL, DistrictID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Enrollment", objectName = "StudentType", body = list(DataObject = body), searchFields = append("StudentTypeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentType
	#'
	#' This function modifies a StudentType
	#' @param fieldNames The field values to give the modified StudentType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The modified StudentType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentType <- function(StudentTypeID, Code = NULL, Description = NULL, DistrictGroupKey = NULL, DistrictID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Enrollment", objectName = "StudentType", objectId = StudentTypeID, body = list(DataObject = body), searchFields = append("StudentTypeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Permits
	#'
	#' This function returns a dataframe or json object of Permits
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Permits. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Permits.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Permit') to get more field paths.
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
	#' @concept Enrollment
	#' @return A list of Permits
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPermits <- function(searchConditionsList = NULL, AllowSchoolPathAssignment = F, Code = F, CodeDescription = F, CreatedTime = F, Description = F, DistrictID = F, ModifiedTime = F, PermitID = F, PermitIDClonedFrom = F, PermitIDClonedTo = F, SchoolYearID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "Permit", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Permit
	#'
	#' This function returns a dataframe or json object of a Permit
	#' @param PermitID The ID of the Permit to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Permit. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Permit.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Permit') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A dataframe or of Permit
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPermit <- function(PermitID, AllowSchoolPathAssignment = F, Code = F, CodeDescription = F, CreatedTime = F, Description = F, DistrictID = F, ModifiedTime = F, PermitIDClonedFrom = F, PermitIDClonedTo = F, SchoolYearID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PermitID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Enrollment", objectName = "Permit", objectId = PermitID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Permit
	#'
	#' This function deletes a Permit
	#' @param PermitID The ID of the Permit to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The PermitID of the deleted Permit.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePermit <- function(PermitID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Enrollment", objectName = "Permit", objectId = PermitID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Permit
	#'
	#' This function creates a Permit
	#' @param fieldNames The field values to give the created Permit. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A newly created Permit
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPermit <- function(AllowSchoolPathAssignment = NULL, Code = NULL, Description = NULL, DistrictID = NULL, PermitIDClonedFrom = NULL, SchoolYearID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Enrollment", objectName = "Permit", body = list(DataObject = body), searchFields = append("PermitID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Permit
	#'
	#' This function modifies a Permit
	#' @param fieldNames The field values to give the modified Permit. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The modified Permit
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPermit <- function(PermitID, AllowSchoolPathAssignment = NULL, Code = NULL, Description = NULL, DistrictID = NULL, PermitIDClonedFrom = NULL, SchoolYearID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Enrollment", objectName = "Permit", objectId = PermitID, body = list(DataObject = body), searchFields = append("PermitID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EntitySchoolBuildings
	#'
	#' This function returns a dataframe or json object of EntitySchoolBuildings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EntitySchoolBuildings. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EntitySchoolBuildings.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EntitySchoolBuilding') to get more field paths.
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
	#' @concept Enrollment
	#' @return A list of EntitySchoolBuildings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEntitySchoolBuildings <- function(searchConditionsList = NULL, BuildingID = F, CreatedTime = F, EntitySchoolBuildingID = F, EntitySchoolBuildingIDClonedFrom = F, EntitySchoolID = F, IsPrimary = F, ModifiedTime = F, SchoolYearID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "EntitySchoolBuilding", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EntitySchoolBuilding
	#'
	#' This function returns a dataframe or json object of an EntitySchoolBuilding
	#' @param EntitySchoolBuildingID The ID of the EntitySchoolBuilding to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EntitySchoolBuilding. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EntitySchoolBuilding.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EntitySchoolBuilding') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A dataframe or of EntitySchoolBuilding
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEntitySchoolBuilding <- function(EntitySchoolBuildingID, BuildingID = F, CreatedTime = F, EntitySchoolBuildingIDClonedFrom = F, EntitySchoolID = F, IsPrimary = F, ModifiedTime = F, SchoolYearID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EntitySchoolBuildingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Enrollment", objectName = "EntitySchoolBuilding", objectId = EntitySchoolBuildingID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EntitySchoolBuilding
	#'
	#' This function deletes an EntitySchoolBuilding
	#' @param EntitySchoolBuildingID The ID of the EntitySchoolBuilding to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The EntitySchoolBuildingID of the deleted EntitySchoolBuilding.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEntitySchoolBuilding <- function(EntitySchoolBuildingID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Enrollment", objectName = "EntitySchoolBuilding", objectId = EntitySchoolBuildingID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EntitySchoolBuilding
	#'
	#' This function creates an EntitySchoolBuilding
	#' @param fieldNames The field values to give the created EntitySchoolBuilding. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A newly created EntitySchoolBuilding
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEntitySchoolBuilding <- function(BuildingID = NULL, EntitySchoolBuildingIDClonedFrom = NULL, EntitySchoolID = NULL, IsPrimary = NULL, SchoolYearID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Enrollment", objectName = "EntitySchoolBuilding", body = list(DataObject = body), searchFields = append("EntitySchoolBuildingID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EntitySchoolBuilding
	#'
	#' This function modifies an EntitySchoolBuilding
	#' @param fieldNames The field values to give the modified EntitySchoolBuilding. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The modified EntitySchoolBuilding
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEntitySchoolBuilding <- function(EntitySchoolBuildingID, BuildingID = NULL, EntitySchoolBuildingIDClonedFrom = NULL, EntitySchoolID = NULL, IsPrimary = NULL, SchoolYearID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Enrollment", objectName = "EntitySchoolBuilding", objectId = EntitySchoolBuildingID, body = list(DataObject = body), searchFields = append("EntitySchoolBuildingID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SchoolPathSchoolOverrides
	#'
	#' This function returns a dataframe or json object of SchoolPathSchoolOverrides
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SchoolPathSchoolOverrides. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SchoolPathSchoolOverrides.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SchoolPathSchoolOverride') to get more field paths.
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
	#' @concept Enrollment
	#' @return A list of SchoolPathSchoolOverrides
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSchoolPathSchoolOverrides <- function(searchConditionsList = NULL, CreatedTime = F, ModifiedTime = F, Order = F, SchoolID = F, SchoolPathSchoolOverrideID = F, StudentID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "SchoolPathSchoolOverride", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SchoolPathSchoolOverride
	#'
	#' This function returns a dataframe or json object of a SchoolPathSchoolOverride
	#' @param SchoolPathSchoolOverrideID The ID of the SchoolPathSchoolOverride to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SchoolPathSchoolOverride. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SchoolPathSchoolOverride.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SchoolPathSchoolOverride') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A dataframe or of SchoolPathSchoolOverride
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSchoolPathSchoolOverride <- function(SchoolPathSchoolOverrideID, CreatedTime = F, ModifiedTime = F, Order = F, SchoolID = F, StudentID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SchoolPathSchoolOverrideID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Enrollment", objectName = "SchoolPathSchoolOverride", objectId = SchoolPathSchoolOverrideID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SchoolPathSchoolOverride
	#'
	#' This function deletes a SchoolPathSchoolOverride
	#' @param SchoolPathSchoolOverrideID The ID of the SchoolPathSchoolOverride to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The SchoolPathSchoolOverrideID of the deleted SchoolPathSchoolOverride.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSchoolPathSchoolOverride <- function(SchoolPathSchoolOverrideID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Enrollment", objectName = "SchoolPathSchoolOverride", objectId = SchoolPathSchoolOverrideID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SchoolPathSchoolOverride
	#'
	#' This function creates a SchoolPathSchoolOverride
	#' @param fieldNames The field values to give the created SchoolPathSchoolOverride. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A newly created SchoolPathSchoolOverride
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSchoolPathSchoolOverride <- function(Order = NULL, SchoolID = NULL, StudentID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Enrollment", objectName = "SchoolPathSchoolOverride", body = list(DataObject = body), searchFields = append("SchoolPathSchoolOverrideID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SchoolPathSchoolOverride
	#'
	#' This function modifies a SchoolPathSchoolOverride
	#' @param fieldNames The field values to give the modified SchoolPathSchoolOverride. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The modified SchoolPathSchoolOverride
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySchoolPathSchoolOverride <- function(SchoolPathSchoolOverrideID, Order = NULL, SchoolID = NULL, StudentID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Enrollment", objectName = "SchoolPathSchoolOverride", objectId = SchoolPathSchoolOverrideID, body = list(DataObject = body), searchFields = append("SchoolPathSchoolOverrideID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempNoShowEntryWithdrawals
	#'
	#' This function returns a dataframe or json object of TempNoShowEntryWithdrawals
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempNoShowEntryWithdrawals. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempNoShowEntryWithdrawals.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempNoShowEntryWithdrawal') to get more field paths.
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
	#' @concept Enrollment
	#' @return A list of TempNoShowEntryWithdrawals
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempNoShowEntryWithdrawals <- function(searchConditionsList = NULL, AttemptToUpdateWithdrawalCode = F, CreatedTime = F, DisplayAction = F, EndDate = F, Entity = F, EntryCode = F, EntryWithdrawalID = F, ErrorCount = F, FailureReason = F, GradeLevel = F, ModifiedTime = F, NoShowAction = F, NoShowEntryWithdrawalType = F, NoShowTypeOfNoShow = F, SchoolYear = F, SchoolYearID = F, StartDate = F, Student = F, StudentID = F, StudentNumber = F, TempNoShowEntryWithdrawalID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, WithdrawalCode = F, WithdrawalCodeID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "TempNoShowEntryWithdrawal", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempNoShowEntryWithdrawal
	#'
	#' This function returns a dataframe or json object of a TempNoShowEntryWithdrawal
	#' @param TempNoShowEntryWithdrawalID The ID of the TempNoShowEntryWithdrawal to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempNoShowEntryWithdrawal. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempNoShowEntryWithdrawal.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempNoShowEntryWithdrawal') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A dataframe or of TempNoShowEntryWithdrawal
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempNoShowEntryWithdrawal <- function(TempNoShowEntryWithdrawalID, AttemptToUpdateWithdrawalCode = F, CreatedTime = F, DisplayAction = F, EndDate = F, Entity = F, EntryCode = F, EntryWithdrawalID = F, ErrorCount = F, FailureReason = F, GradeLevel = F, ModifiedTime = F, NoShowAction = F, NoShowEntryWithdrawalType = F, NoShowTypeOfNoShow = F, SchoolYear = F, SchoolYearID = F, StartDate = F, Student = F, StudentID = F, StudentNumber = F, UserIDCreatedBy = F, UserIDModifiedBy = F, WithdrawalCode = F, WithdrawalCodeID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempNoShowEntryWithdrawalID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Enrollment", objectName = "TempNoShowEntryWithdrawal", objectId = TempNoShowEntryWithdrawalID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempNoShowEntryWithdrawal
	#'
	#' This function deletes a TempNoShowEntryWithdrawal
	#' @param TempNoShowEntryWithdrawalID The ID of the TempNoShowEntryWithdrawal to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The TempNoShowEntryWithdrawalID of the deleted TempNoShowEntryWithdrawal.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempNoShowEntryWithdrawal <- function(TempNoShowEntryWithdrawalID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Enrollment", objectName = "TempNoShowEntryWithdrawal", objectId = TempNoShowEntryWithdrawalID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempNoShowEntryWithdrawal
	#'
	#' This function creates a TempNoShowEntryWithdrawal
	#' @param fieldNames The field values to give the created TempNoShowEntryWithdrawal. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A newly created TempNoShowEntryWithdrawal
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempNoShowEntryWithdrawal <- function(AttemptToUpdateWithdrawalCode = NULL, DisplayAction = NULL, EndDate = NULL, Entity = NULL, EntryCode = NULL, EntryWithdrawalID = NULL, ErrorCount = NULL, FailureReason = NULL, GradeLevel = NULL, NoShowAction = NULL, NoShowEntryWithdrawalType = NULL, NoShowTypeOfNoShow = NULL, SchoolYear = NULL, SchoolYearID = NULL, StartDate = NULL, Student = NULL, StudentID = NULL, StudentNumber = NULL, WithdrawalCode = NULL, WithdrawalCodeID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Enrollment", objectName = "TempNoShowEntryWithdrawal", body = list(DataObject = body), searchFields = append("TempNoShowEntryWithdrawalID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempNoShowEntryWithdrawal
	#'
	#' This function modifies a TempNoShowEntryWithdrawal
	#' @param fieldNames The field values to give the modified TempNoShowEntryWithdrawal. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The modified TempNoShowEntryWithdrawal
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempNoShowEntryWithdrawal <- function(TempNoShowEntryWithdrawalID, AttemptToUpdateWithdrawalCode = NULL, DisplayAction = NULL, EndDate = NULL, Entity = NULL, EntryCode = NULL, EntryWithdrawalID = NULL, ErrorCount = NULL, FailureReason = NULL, GradeLevel = NULL, NoShowAction = NULL, NoShowEntryWithdrawalType = NULL, NoShowTypeOfNoShow = NULL, SchoolYear = NULL, SchoolYearID = NULL, StartDate = NULL, Student = NULL, StudentID = NULL, StudentNumber = NULL, WithdrawalCode = NULL, WithdrawalCodeID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Enrollment", objectName = "TempNoShowEntryWithdrawal", objectId = TempNoShowEntryWithdrawalID, body = list(DataObject = body), searchFields = append("TempNoShowEntryWithdrawalID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempNameAddressMoveSchoolPathSchoolOverrides
	#'
	#' This function returns a dataframe or json object of TempNameAddressMoveSchoolPathSchoolOverrides
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempNameAddressMoveSchoolPathSchoolOverrides. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempNameAddressMoveSchoolPathSchoolOverrides.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempNameAddressMoveSchoolPathSchoolOverride') to get more field paths.
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
	#' @concept Enrollment
	#' @return A list of TempNameAddressMoveSchoolPathSchoolOverrides
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempNameAddressMoveSchoolPathSchoolOverrides <- function(searchConditionsList = NULL, CreatedTime = F, IsCreateOverride = F, IsOverrideExists = F, IsPermitOptional = F, IsRemoveOverride = F, IsRemovePermit = F, IsUpdateOverride = F, ModifiedTime = F, Order = F, PermitID = F, PermitSchoolYearID = F, SchoolID = F, SchoolNameOverriddingTo = F, SchoolNameToOverride = F, SchoolPathSchoolOverrideID = F, SchoolYearDescription = F, StudentFullNameLFM = F, StudentID = F, TempNameAddressMoveSchoolPathSchoolOverrideID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "TempNameAddressMoveSchoolPathSchoolOverride", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempNameAddressMoveSchoolPathSchoolOverride
	#'
	#' This function returns a dataframe or json object of a TempNameAddressMoveSchoolPathSchoolOverride
	#' @param TempNameAddressMoveSchoolPathSchoolOverrideID The ID of the TempNameAddressMoveSchoolPathSchoolOverride to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempNameAddressMoveSchoolPathSchoolOverride. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempNameAddressMoveSchoolPathSchoolOverride.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempNameAddressMoveSchoolPathSchoolOverride') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A dataframe or of TempNameAddressMoveSchoolPathSchoolOverride
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempNameAddressMoveSchoolPathSchoolOverride <- function(TempNameAddressMoveSchoolPathSchoolOverrideID, CreatedTime = F, IsCreateOverride = F, IsOverrideExists = F, IsPermitOptional = F, IsRemoveOverride = F, IsRemovePermit = F, IsUpdateOverride = F, ModifiedTime = F, Order = F, PermitID = F, PermitSchoolYearID = F, SchoolID = F, SchoolNameOverriddingTo = F, SchoolNameToOverride = F, SchoolPathSchoolOverrideID = F, SchoolYearDescription = F, StudentFullNameLFM = F, StudentID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempNameAddressMoveSchoolPathSchoolOverrideID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Enrollment", objectName = "TempNameAddressMoveSchoolPathSchoolOverride", objectId = TempNameAddressMoveSchoolPathSchoolOverrideID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempNameAddressMoveSchoolPathSchoolOverride
	#'
	#' This function deletes a TempNameAddressMoveSchoolPathSchoolOverride
	#' @param TempNameAddressMoveSchoolPathSchoolOverrideID The ID of the TempNameAddressMoveSchoolPathSchoolOverride to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The TempNameAddressMoveSchoolPathSchoolOverrideID of the deleted TempNameAddressMoveSchoolPathSchoolOverride.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempNameAddressMoveSchoolPathSchoolOverride <- function(TempNameAddressMoveSchoolPathSchoolOverrideID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Enrollment", objectName = "TempNameAddressMoveSchoolPathSchoolOverride", objectId = TempNameAddressMoveSchoolPathSchoolOverrideID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempNameAddressMoveSchoolPathSchoolOverride
	#'
	#' This function creates a TempNameAddressMoveSchoolPathSchoolOverride
	#' @param fieldNames The field values to give the created TempNameAddressMoveSchoolPathSchoolOverride. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A newly created TempNameAddressMoveSchoolPathSchoolOverride
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempNameAddressMoveSchoolPathSchoolOverride <- function(IsCreateOverride = NULL, IsOverrideExists = NULL, IsPermitOptional = NULL, IsRemoveOverride = NULL, IsRemovePermit = NULL, IsUpdateOverride = NULL, Order = NULL, PermitID = NULL, PermitSchoolYearID = NULL, SchoolID = NULL, SchoolNameOverriddingTo = NULL, SchoolNameToOverride = NULL, SchoolPathSchoolOverrideID = NULL, SchoolYearDescription = NULL, StudentFullNameLFM = NULL, StudentID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Enrollment", objectName = "TempNameAddressMoveSchoolPathSchoolOverride", body = list(DataObject = body), searchFields = append("TempNameAddressMoveSchoolPathSchoolOverrideID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempNameAddressMoveSchoolPathSchoolOverride
	#'
	#' This function modifies a TempNameAddressMoveSchoolPathSchoolOverride
	#' @param fieldNames The field values to give the modified TempNameAddressMoveSchoolPathSchoolOverride. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The modified TempNameAddressMoveSchoolPathSchoolOverride
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempNameAddressMoveSchoolPathSchoolOverride <- function(TempNameAddressMoveSchoolPathSchoolOverrideID, IsCreateOverride = NULL, IsOverrideExists = NULL, IsPermitOptional = NULL, IsRemoveOverride = NULL, IsRemovePermit = NULL, IsUpdateOverride = NULL, Order = NULL, PermitID = NULL, PermitSchoolYearID = NULL, SchoolID = NULL, SchoolNameOverriddingTo = NULL, SchoolNameToOverride = NULL, SchoolPathSchoolOverrideID = NULL, SchoolYearDescription = NULL, StudentFullNameLFM = NULL, StudentID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Enrollment", objectName = "TempNameAddressMoveSchoolPathSchoolOverride", objectId = TempNameAddressMoveSchoolPathSchoolOverrideID, body = list(DataObject = body), searchFields = append("TempNameAddressMoveSchoolPathSchoolOverrideID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempSchoolPathSchoolOverrides
	#'
	#' This function returns a dataframe or json object of TempSchoolPathSchoolOverrides
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempSchoolPathSchoolOverrides. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempSchoolPathSchoolOverrides.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempSchoolPathSchoolOverride') to get more field paths.
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
	#' @concept Enrollment
	#' @return A list of TempSchoolPathSchoolOverrides
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempSchoolPathSchoolOverrides <- function(searchConditionsList = NULL, CreatedTime = F, DistrictID = F, ExceptionNote = F, HasExceptions = F, ModifiedTime = F, Order = F, PermitCodeDescription = F, PermitID = F, SchoolCodeName = F, SchoolID = F, SchoolIDClonedTo = F, SchoolYearID = F, StudentID = F, StudentName = F, TempSchoolPathSchoolOverrideID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "TempSchoolPathSchoolOverride", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempSchoolPathSchoolOverride
	#'
	#' This function returns a dataframe or json object of a TempSchoolPathSchoolOverride
	#' @param TempSchoolPathSchoolOverrideID The ID of the TempSchoolPathSchoolOverride to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempSchoolPathSchoolOverride. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempSchoolPathSchoolOverride.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempSchoolPathSchoolOverride') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A dataframe or of TempSchoolPathSchoolOverride
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempSchoolPathSchoolOverride <- function(TempSchoolPathSchoolOverrideID, CreatedTime = F, DistrictID = F, ExceptionNote = F, HasExceptions = F, ModifiedTime = F, Order = F, PermitCodeDescription = F, PermitID = F, SchoolCodeName = F, SchoolID = F, SchoolIDClonedTo = F, SchoolYearID = F, StudentID = F, StudentName = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempSchoolPathSchoolOverrideID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Enrollment", objectName = "TempSchoolPathSchoolOverride", objectId = TempSchoolPathSchoolOverrideID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempSchoolPathSchoolOverride
	#'
	#' This function deletes a TempSchoolPathSchoolOverride
	#' @param TempSchoolPathSchoolOverrideID The ID of the TempSchoolPathSchoolOverride to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The TempSchoolPathSchoolOverrideID of the deleted TempSchoolPathSchoolOverride.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempSchoolPathSchoolOverride <- function(TempSchoolPathSchoolOverrideID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Enrollment", objectName = "TempSchoolPathSchoolOverride", objectId = TempSchoolPathSchoolOverrideID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempSchoolPathSchoolOverride
	#'
	#' This function creates a TempSchoolPathSchoolOverride
	#' @param fieldNames The field values to give the created TempSchoolPathSchoolOverride. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A newly created TempSchoolPathSchoolOverride
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempSchoolPathSchoolOverride <- function(DistrictID = NULL, ExceptionNote = NULL, HasExceptions = NULL, Order = NULL, PermitCodeDescription = NULL, PermitID = NULL, SchoolCodeName = NULL, SchoolID = NULL, SchoolIDClonedTo = NULL, SchoolYearID = NULL, StudentID = NULL, StudentName = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Enrollment", objectName = "TempSchoolPathSchoolOverride", body = list(DataObject = body), searchFields = append("TempSchoolPathSchoolOverrideID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempSchoolPathSchoolOverride
	#'
	#' This function modifies a TempSchoolPathSchoolOverride
	#' @param fieldNames The field values to give the modified TempSchoolPathSchoolOverride. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The modified TempSchoolPathSchoolOverride
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempSchoolPathSchoolOverride <- function(TempSchoolPathSchoolOverrideID, DistrictID = NULL, ExceptionNote = NULL, HasExceptions = NULL, Order = NULL, PermitCodeDescription = NULL, PermitID = NULL, SchoolCodeName = NULL, SchoolID = NULL, SchoolIDClonedTo = NULL, SchoolYearID = NULL, StudentID = NULL, StudentName = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Enrollment", objectName = "TempSchoolPathSchoolOverride", objectId = TempSchoolPathSchoolOverrideID, body = list(DataObject = body), searchFields = append("TempSchoolPathSchoolOverrideID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempHomeroomErrors
	#'
	#' This function returns a dataframe or json object of TempHomeroomErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempHomeroomErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempHomeroomErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempHomeroomError') to get more field paths.
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
	#' @concept Enrollment
	#' @return A list of TempHomeroomErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempHomeroomErrors <- function(searchConditionsList = NULL, Code = F, CreatedTime = F, FailureReason = F, ModifiedTime = F, TempHomeroomErrorID = F, TempHomeroomRecordID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "TempHomeroomError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempHomeroomError
	#'
	#' This function returns a dataframe or json object of a TempHomeroomError
	#' @param TempHomeroomErrorID The ID of the TempHomeroomError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempHomeroomError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempHomeroomError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempHomeroomError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A dataframe or of TempHomeroomError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempHomeroomError <- function(TempHomeroomErrorID, Code = F, CreatedTime = F, FailureReason = F, ModifiedTime = F, TempHomeroomRecordID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempHomeroomErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Enrollment", objectName = "TempHomeroomError", objectId = TempHomeroomErrorID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempHomeroomError
	#'
	#' This function deletes a TempHomeroomError
	#' @param TempHomeroomErrorID The ID of the TempHomeroomError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The TempHomeroomErrorID of the deleted TempHomeroomError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempHomeroomError <- function(TempHomeroomErrorID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Enrollment", objectName = "TempHomeroomError", objectId = TempHomeroomErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempHomeroomError
	#'
	#' This function creates a TempHomeroomError
	#' @param fieldNames The field values to give the created TempHomeroomError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A newly created TempHomeroomError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempHomeroomError <- function(Code = NULL, FailureReason = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Enrollment", objectName = "TempHomeroomError", body = list(DataObject = body), searchFields = append("TempHomeroomErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempHomeroomError
	#'
	#' This function modifies a TempHomeroomError
	#' @param fieldNames The field values to give the modified TempHomeroomError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The modified TempHomeroomError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempHomeroomError <- function(TempHomeroomErrorID, Code = NULL, FailureReason = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Enrollment", objectName = "TempHomeroomError", objectId = TempHomeroomErrorID, body = list(DataObject = body), searchFields = append("TempHomeroomErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempHomeroomRecords
	#'
	#' This function returns a dataframe or json object of TempHomeroomRecords
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempHomeroomRecords. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempHomeroomRecords.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempHomeroomRecord') to get more field paths.
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
	#' @concept Enrollment
	#' @return A list of TempHomeroomRecords
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempHomeroomRecords <- function(searchConditionsList = NULL, Building = F, BuildingID = F, Code = F, ColumnIndex = F, CreatedTime = F, HasSaveError = F, HomeroomID = F, IsOverwrite = F, ModifiedTime = F, Room = F, RoomID = F, SchoolYear = F, SchoolYearID = F, Staff = F, StaffID = F, TempHomeroomRecordID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "TempHomeroomRecord", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempHomeroomRecord
	#'
	#' This function returns a dataframe or json object of a TempHomeroomRecord
	#' @param TempHomeroomRecordID The ID of the TempHomeroomRecord to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempHomeroomRecord. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempHomeroomRecord.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempHomeroomRecord') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A dataframe or of TempHomeroomRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempHomeroomRecord <- function(TempHomeroomRecordID, Building = F, BuildingID = F, Code = F, ColumnIndex = F, CreatedTime = F, HasSaveError = F, HomeroomID = F, IsOverwrite = F, ModifiedTime = F, Room = F, RoomID = F, SchoolYear = F, SchoolYearID = F, Staff = F, StaffID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempHomeroomRecordID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Enrollment", objectName = "TempHomeroomRecord", objectId = TempHomeroomRecordID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempHomeroomRecord
	#'
	#' This function deletes a TempHomeroomRecord
	#' @param TempHomeroomRecordID The ID of the TempHomeroomRecord to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The TempHomeroomRecordID of the deleted TempHomeroomRecord.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempHomeroomRecord <- function(TempHomeroomRecordID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Enrollment", objectName = "TempHomeroomRecord", objectId = TempHomeroomRecordID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempHomeroomRecord
	#'
	#' This function creates a TempHomeroomRecord
	#' @param fieldNames The field values to give the created TempHomeroomRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A newly created TempHomeroomRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempHomeroomRecord <- function(Building = NULL, BuildingID = NULL, Code = NULL, HasSaveError = NULL, HomeroomID = NULL, IsOverwrite = NULL, Room = NULL, RoomID = NULL, SchoolYear = NULL, SchoolYearID = NULL, Staff = NULL, StaffID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Enrollment", objectName = "TempHomeroomRecord", body = list(DataObject = body), searchFields = append("TempHomeroomRecordID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempHomeroomRecord
	#'
	#' This function modifies a TempHomeroomRecord
	#' @param fieldNames The field values to give the modified TempHomeroomRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The modified TempHomeroomRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempHomeroomRecord <- function(TempHomeroomRecordID, Building = NULL, BuildingID = NULL, Code = NULL, HasSaveError = NULL, HomeroomID = NULL, IsOverwrite = NULL, Room = NULL, RoomID = NULL, SchoolYear = NULL, SchoolYearID = NULL, Staff = NULL, StaffID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Enrollment", objectName = "TempHomeroomRecord", objectId = TempHomeroomRecordID, body = list(DataObject = body), searchFields = append("TempHomeroomRecordID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentAccountsMAS
	#'
	#' This function returns a dataframe or json object of StudentAccountsMAS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentAccountsMAS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentAccountsMAS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentAccountsMA') to get more field paths.
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
	#' @concept Enrollment
	#' @return A list of StudentAccountsMAS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentAccountsMAS <- function(searchConditionsList = NULL, AMTransportationID = F, EthnicityMAID = F, FacultyStaffChild = F, FinancialAid = F, iPadLease = F, NYDepositPaid = F, PaymentPlanMAID = F, PlaceofWorship = F, PMTransportationID = F, ReligionID = F, SchoolDistrictID = F, StudentAccountsMAID = F, StudentID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "StudentAccountsMA", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentAccountsMA
	#'
	#' This function returns a dataframe or json object of a StudentAccountsMA
	#' @param StudentAccountsMAID The ID of the StudentAccountsMA to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentAccountsMA. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentAccountsMA.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentAccountsMA') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A dataframe or of StudentAccountsMA
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentAccountsMA <- function(StudentAccountsMAID, AMTransportationID = F, EthnicityMAID = F, FacultyStaffChild = F, FinancialAid = F, iPadLease = F, NYDepositPaid = F, PaymentPlanMAID = F, PlaceofWorship = F, PMTransportationID = F, ReligionID = F, SchoolDistrictID = F, StudentID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentAccountsMAID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Enrollment", objectName = "StudentAccountsMA", objectId = StudentAccountsMAID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentAccountsMA
	#'
	#' This function deletes a StudentAccountsMA
	#' @param StudentAccountsMAID The ID of the StudentAccountsMA to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The StudentAccountsMAID of the deleted StudentAccountsMA.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentAccountsMA <- function(StudentAccountsMAID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Enrollment", objectName = "StudentAccountsMA", objectId = StudentAccountsMAID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentAccountsMA
	#'
	#' This function creates a StudentAccountsMA
	#' @param fieldNames The field values to give the created StudentAccountsMA. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A newly created StudentAccountsMA
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentAccountsMA <- function(AMTransportationID = NULL, EthnicityMAID = NULL, FacultyStaffChild = NULL, FinancialAid = NULL, iPadLease = NULL, NYDepositPaid = NULL, PaymentPlanMAID = NULL, PlaceofWorship = NULL, PMTransportationID = NULL, ReligionID = NULL, SchoolDistrictID = NULL, StudentID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Enrollment", objectName = "StudentAccountsMA", body = list(DataObject = body), searchFields = append("StudentAccountsMAID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentAccountsMA
	#'
	#' This function modifies a StudentAccountsMA
	#' @param fieldNames The field values to give the modified StudentAccountsMA. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The modified StudentAccountsMA
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentAccountsMA <- function(StudentAccountsMAID, AMTransportationID = NULL, EthnicityMAID = NULL, FacultyStaffChild = NULL, FinancialAid = NULL, iPadLease = NULL, NYDepositPaid = NULL, PaymentPlanMAID = NULL, PlaceofWorship = NULL, PMTransportationID = NULL, ReligionID = NULL, SchoolDistrictID = NULL, StudentID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Enrollment", objectName = "StudentAccountsMA", objectId = StudentAccountsMAID, body = list(DataObject = body), searchFields = append("StudentAccountsMAID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PaymentPlanMAS
	#'
	#' This function returns a dataframe or json object of PaymentPlanMAS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PaymentPlanMAS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PaymentPlanMAS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PaymentPlanMA') to get more field paths.
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
	#' @concept Enrollment
	#' @return A list of PaymentPlanMAS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPaymentPlanMAS <- function(searchConditionsList = NULL, Code = F, Description = F, PaymentPlanMAID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "PaymentPlanMA", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PaymentPlanMA
	#'
	#' This function returns a dataframe or json object of a PaymentPlanMA
	#' @param PaymentPlanMAID The ID of the PaymentPlanMA to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PaymentPlanMA. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PaymentPlanMA.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PaymentPlanMA') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A dataframe or of PaymentPlanMA
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPaymentPlanMA <- function(PaymentPlanMAID, Code = F, Description = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PaymentPlanMAID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Enrollment", objectName = "PaymentPlanMA", objectId = PaymentPlanMAID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PaymentPlanMA
	#'
	#' This function deletes a PaymentPlanMA
	#' @param PaymentPlanMAID The ID of the PaymentPlanMA to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The PaymentPlanMAID of the deleted PaymentPlanMA.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePaymentPlanMA <- function(PaymentPlanMAID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Enrollment", objectName = "PaymentPlanMA", objectId = PaymentPlanMAID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PaymentPlanMA
	#'
	#' This function creates a PaymentPlanMA
	#' @param fieldNames The field values to give the created PaymentPlanMA. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A newly created PaymentPlanMA
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPaymentPlanMA <- function(Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Enrollment", objectName = "PaymentPlanMA", body = list(DataObject = body), searchFields = append("PaymentPlanMAID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PaymentPlanMA
	#'
	#' This function modifies a PaymentPlanMA
	#' @param fieldNames The field values to give the modified PaymentPlanMA. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The modified PaymentPlanMA
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPaymentPlanMA <- function(PaymentPlanMAID, Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Enrollment", objectName = "PaymentPlanMA", objectId = PaymentPlanMAID, body = list(DataObject = body), searchFields = append("PaymentPlanMAID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List NumberedStudentEntityYearForDistrictAndSchoolYears
	#'
	#' This function returns a dataframe or json object of NumberedStudentEntityYearForDistrictAndSchoolYears
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NumberedStudentEntityYearForDistrictAndSchoolYears. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NumberedStudentEntityYearForDistrictAndSchoolYears.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NumberedStudentEntityYearForDistrictAndSchoolYear') to get more field paths.
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
	#' @concept Enrollment
	#' @return A list of NumberedStudentEntityYearForDistrictAndSchoolYears
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNumberedStudentEntityYearForDistrictAndSchoolYears <- function(searchConditionsList = NULL, DistrictID = F, EntityID = F, IsDefaultEntity = F, SchoolYearID = F, StudentDistrictRowNumber = F, StudentEntityYearID = F, StudentID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "NumberedStudentEntityYearForDistrictAndSchoolYear", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NumberedStudentEntityYearForDistrictAndSchoolYear
	#'
	#' This function returns a dataframe or json object of a NumberedStudentEntityYearForDistrictAndSchoolYear
	#' @param NumberedStudentEntityYearForDistrictAndSchoolYearID The ID of the NumberedStudentEntityYearForDistrictAndSchoolYear to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NumberedStudentEntityYearForDistrictAndSchoolYear. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NumberedStudentEntityYearForDistrictAndSchoolYear.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NumberedStudentEntityYearForDistrictAndSchoolYear') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A dataframe or of NumberedStudentEntityYearForDistrictAndSchoolYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNumberedStudentEntityYearForDistrictAndSchoolYear <- function(NumberedStudentEntityYearForDistrictAndSchoolYearID, DistrictID = F, EntityID = F, IsDefaultEntity = F, SchoolYearID = F, StudentDistrictRowNumber = F, StudentEntityYearID = F, StudentID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NumberedStudentEntityYearForDistrictAndSchoolYearID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Enrollment", objectName = "NumberedStudentEntityYearForDistrictAndSchoolYear", objectId = NumberedStudentEntityYearForDistrictAndSchoolYearID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NumberedStudentEntityYearForDistrictAndSchoolYear
	#'
	#' This function deletes a NumberedStudentEntityYearForDistrictAndSchoolYear
	#' @param NumberedStudentEntityYearForDistrictAndSchoolYearID The ID of the NumberedStudentEntityYearForDistrictAndSchoolYear to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The NumberedStudentEntityYearForDistrictAndSchoolYearID of the deleted NumberedStudentEntityYearForDistrictAndSchoolYear.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNumberedStudentEntityYearForDistrictAndSchoolYear <- function(NumberedStudentEntityYearForDistrictAndSchoolYearID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Enrollment", objectName = "NumberedStudentEntityYearForDistrictAndSchoolYear", objectId = NumberedStudentEntityYearForDistrictAndSchoolYearID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CommonEducationDataStandardsGradeLevels
	#'
	#' This function returns a dataframe or json object of CommonEducationDataStandardsGradeLevels
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CommonEducationDataStandardsGradeLevels. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CommonEducationDataStandardsGradeLevels.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CommonEducationDataStandardsGradeLevel') to get more field paths.
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
	#' @concept Enrollment
	#' @return A list of CommonEducationDataStandardsGradeLevels
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCommonEducationDataStandardsGradeLevels <- function(searchConditionsList = NULL, Code = F, CommonEducationDataStandardsGradeLevelID = F, CreatedTime = F, Description = F, ModifiedTime = F, OrderNumber = F, SkywardHash = F, SkywardID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "CommonEducationDataStandardsGradeLevel", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CommonEducationDataStandardsGradeLevel
	#'
	#' This function returns a dataframe or json object of a CommonEducationDataStandardsGradeLevel
	#' @param CommonEducationDataStandardsGradeLevelID The ID of the CommonEducationDataStandardsGradeLevel to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CommonEducationDataStandardsGradeLevel. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CommonEducationDataStandardsGradeLevel.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CommonEducationDataStandardsGradeLevel') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A dataframe or of CommonEducationDataStandardsGradeLevel
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCommonEducationDataStandardsGradeLevel <- function(CommonEducationDataStandardsGradeLevelID, Code = F, CreatedTime = F, Description = F, ModifiedTime = F, OrderNumber = F, SkywardHash = F, SkywardID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CommonEducationDataStandardsGradeLevelID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Enrollment", objectName = "CommonEducationDataStandardsGradeLevel", objectId = CommonEducationDataStandardsGradeLevelID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CommonEducationDataStandardsGradeLevel
	#'
	#' This function deletes a CommonEducationDataStandardsGradeLevel
	#' @param CommonEducationDataStandardsGradeLevelID The ID of the CommonEducationDataStandardsGradeLevel to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The CommonEducationDataStandardsGradeLevelID of the deleted CommonEducationDataStandardsGradeLevel.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCommonEducationDataStandardsGradeLevel <- function(CommonEducationDataStandardsGradeLevelID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Enrollment", objectName = "CommonEducationDataStandardsGradeLevel", objectId = CommonEducationDataStandardsGradeLevelID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CommonEducationDataStandardsGradeLevel
	#'
	#' This function creates a CommonEducationDataStandardsGradeLevel
	#' @param fieldNames The field values to give the created CommonEducationDataStandardsGradeLevel. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A newly created CommonEducationDataStandardsGradeLevel
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCommonEducationDataStandardsGradeLevel <- function(Code = NULL, Description = NULL, OrderNumber = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Enrollment", objectName = "CommonEducationDataStandardsGradeLevel", body = list(DataObject = body), searchFields = append("CommonEducationDataStandardsGradeLevelID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CommonEducationDataStandardsGradeLevel
	#'
	#' This function modifies a CommonEducationDataStandardsGradeLevel
	#' @param fieldNames The field values to give the modified CommonEducationDataStandardsGradeLevel. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The modified CommonEducationDataStandardsGradeLevel
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCommonEducationDataStandardsGradeLevel <- function(CommonEducationDataStandardsGradeLevelID, Code = NULL, Description = NULL, OrderNumber = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Enrollment", objectName = "CommonEducationDataStandardsGradeLevel", objectId = CommonEducationDataStandardsGradeLevelID, body = list(DataObject = body), searchFields = append("CommonEducationDataStandardsGradeLevelID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempStudentEnrollmentErrors
	#'
	#' This function returns a dataframe or json object of TempStudentEnrollmentErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentEnrollmentErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentEnrollmentErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentEnrollmentError') to get more field paths.
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
	#' @concept Enrollment
	#' @return A list of TempStudentEnrollmentErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStudentEnrollmentErrors <- function(searchConditionsList = NULL, CreatedTime = F, Error = F, ErrorCount = F, ErrorDetail = F, ModifiedTime = F, TempStudentEnrollmentErrorID = F, TempStudentEnrollmentRecordID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "TempStudentEnrollmentError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempStudentEnrollmentError
	#'
	#' This function returns a dataframe or json object of a TempStudentEnrollmentError
	#' @param TempStudentEnrollmentErrorID The ID of the TempStudentEnrollmentError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentEnrollmentError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentEnrollmentError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentEnrollmentError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A dataframe or of TempStudentEnrollmentError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempStudentEnrollmentError <- function(TempStudentEnrollmentErrorID, CreatedTime = F, Error = F, ErrorCount = F, ErrorDetail = F, ModifiedTime = F, TempStudentEnrollmentRecordID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempStudentEnrollmentErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Enrollment", objectName = "TempStudentEnrollmentError", objectId = TempStudentEnrollmentErrorID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempStudentEnrollmentError
	#'
	#' This function deletes a TempStudentEnrollmentError
	#' @param TempStudentEnrollmentErrorID The ID of the TempStudentEnrollmentError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The TempStudentEnrollmentErrorID of the deleted TempStudentEnrollmentError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempStudentEnrollmentError <- function(TempStudentEnrollmentErrorID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Enrollment", objectName = "TempStudentEnrollmentError", objectId = TempStudentEnrollmentErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempStudentEnrollmentError
	#'
	#' This function creates a TempStudentEnrollmentError
	#' @param fieldNames The field values to give the created TempStudentEnrollmentError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A newly created TempStudentEnrollmentError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempStudentEnrollmentError <- function(Error = NULL, ErrorCount = NULL, ErrorDetail = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Enrollment", objectName = "TempStudentEnrollmentError", body = list(DataObject = body), searchFields = append("TempStudentEnrollmentErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempStudentEnrollmentError
	#'
	#' This function modifies a TempStudentEnrollmentError
	#' @param fieldNames The field values to give the modified TempStudentEnrollmentError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The modified TempStudentEnrollmentError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempStudentEnrollmentError <- function(TempStudentEnrollmentErrorID, Error = NULL, ErrorCount = NULL, ErrorDetail = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Enrollment", objectName = "TempStudentEnrollmentError", objectId = TempStudentEnrollmentErrorID, body = list(DataObject = body), searchFields = append("TempStudentEnrollmentErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentPermits
	#'
	#' This function returns a dataframe or json object of StudentPermits
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentPermits. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentPermits.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentPermit') to get more field paths.
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
	#' @concept Enrollment
	#' @return A list of StudentPermits
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentPermits <- function(searchConditionsList = NULL, CreatedTime = F, DistrictID = F, ModifiedTime = F, PermitID = F, SchoolYearID = F, StudentID = F, StudentPermitID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "StudentPermit", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentPermit
	#'
	#' This function returns a dataframe or json object of a StudentPermit
	#' @param StudentPermitID The ID of the StudentPermit to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentPermit. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentPermit.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentPermit') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A dataframe or of StudentPermit
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentPermit <- function(StudentPermitID, CreatedTime = F, DistrictID = F, ModifiedTime = F, PermitID = F, SchoolYearID = F, StudentID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentPermitID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Enrollment", objectName = "StudentPermit", objectId = StudentPermitID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentPermit
	#'
	#' This function deletes a StudentPermit
	#' @param StudentPermitID The ID of the StudentPermit to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The StudentPermitID of the deleted StudentPermit.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentPermit <- function(StudentPermitID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Enrollment", objectName = "StudentPermit", objectId = StudentPermitID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentPermit
	#'
	#' This function creates a StudentPermit
	#' @param fieldNames The field values to give the created StudentPermit. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A newly created StudentPermit
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentPermit <- function(DistrictID = NULL, PermitID = NULL, SchoolYearID = NULL, StudentID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Enrollment", objectName = "StudentPermit", body = list(DataObject = body), searchFields = append("StudentPermitID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentPermit
	#'
	#' This function modifies a StudentPermit
	#' @param fieldNames The field values to give the modified StudentPermit. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The modified StudentPermit
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentPermit <- function(StudentPermitID, DistrictID = NULL, PermitID = NULL, SchoolYearID = NULL, StudentID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Enrollment", objectName = "StudentPermit", objectId = StudentPermitID, body = list(DataObject = body), searchFields = append("StudentPermitID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AMTransportations
	#'
	#' This function returns a dataframe or json object of AMTransportations
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AMTransportations. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AMTransportations.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AMTransportation') to get more field paths.
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
	#' @concept Enrollment
	#' @return A list of AMTransportations
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAMTransportations <- function(searchConditionsList = NULL, AMTransportationID = F, Code = F, Description = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "AMTransportation", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AMTransportation
	#'
	#' This function returns a dataframe or json object of an AMTransportation
	#' @param AMTransportationID The ID of the AMTransportation to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AMTransportation. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AMTransportation.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AMTransportation') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A dataframe or of AMTransportation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAMTransportation <- function(AMTransportationID, Code = F, Description = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AMTransportationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Enrollment", objectName = "AMTransportation", objectId = AMTransportationID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AMTransportation
	#'
	#' This function deletes an AMTransportation
	#' @param AMTransportationID The ID of the AMTransportation to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The AMTransportationID of the deleted AMTransportation.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAMTransportation <- function(AMTransportationID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Enrollment", objectName = "AMTransportation", objectId = AMTransportationID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AMTransportation
	#'
	#' This function creates an AMTransportation
	#' @param fieldNames The field values to give the created AMTransportation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A newly created AMTransportation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAMTransportation <- function(Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Enrollment", objectName = "AMTransportation", body = list(DataObject = body), searchFields = append("AMTransportationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AMTransportation
	#'
	#' This function modifies an AMTransportation
	#' @param fieldNames The field values to give the modified AMTransportation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The modified AMTransportation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAMTransportation <- function(AMTransportationID, Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Enrollment", objectName = "AMTransportation", objectId = AMTransportationID, body = list(DataObject = body), searchFields = append("AMTransportationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PMTransportations
	#'
	#' This function returns a dataframe or json object of PMTransportations
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PMTransportations. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PMTransportations.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PMTransportation') to get more field paths.
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
	#' @concept Enrollment
	#' @return A list of PMTransportations
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPMTransportations <- function(searchConditionsList = NULL, Code = F, Description = F, PMTransportationID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "PMTransportation", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PMTransportation
	#'
	#' This function returns a dataframe or json object of a PMTransportation
	#' @param PMTransportationID The ID of the PMTransportation to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PMTransportation. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PMTransportation.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PMTransportation') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A dataframe or of PMTransportation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPMTransportation <- function(PMTransportationID, Code = F, Description = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PMTransportationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Enrollment", objectName = "PMTransportation", objectId = PMTransportationID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PMTransportation
	#'
	#' This function deletes a PMTransportation
	#' @param PMTransportationID The ID of the PMTransportation to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The PMTransportationID of the deleted PMTransportation.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePMTransportation <- function(PMTransportationID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Enrollment", objectName = "PMTransportation", objectId = PMTransportationID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PMTransportation
	#'
	#' This function creates a PMTransportation
	#' @param fieldNames The field values to give the created PMTransportation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A newly created PMTransportation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPMTransportation <- function(Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Enrollment", objectName = "PMTransportation", body = list(DataObject = body), searchFields = append("PMTransportationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PMTransportation
	#'
	#' This function modifies a PMTransportation
	#' @param fieldNames The field values to give the modified PMTransportation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The modified PMTransportation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPMTransportation <- function(PMTransportationID, Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Enrollment", objectName = "PMTransportation", objectId = PMTransportationID, body = list(DataObject = body), searchFields = append("PMTransportationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SchoolDistricts
	#'
	#' This function returns a dataframe or json object of SchoolDistricts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SchoolDistricts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SchoolDistricts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SchoolDistrict') to get more field paths.
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
	#' @concept Enrollment
	#' @return A list of SchoolDistricts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSchoolDistricts <- function(searchConditionsList = NULL, Code = F, Description = F, SchoolDistrictID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "SchoolDistrict", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SchoolDistrict
	#'
	#' This function returns a dataframe or json object of a SchoolDistrict
	#' @param SchoolDistrictID The ID of the SchoolDistrict to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SchoolDistrict. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SchoolDistrict.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SchoolDistrict') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A dataframe or of SchoolDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSchoolDistrict <- function(SchoolDistrictID, Code = F, Description = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SchoolDistrictID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Enrollment", objectName = "SchoolDistrict", objectId = SchoolDistrictID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SchoolDistrict
	#'
	#' This function deletes a SchoolDistrict
	#' @param SchoolDistrictID The ID of the SchoolDistrict to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The SchoolDistrictID of the deleted SchoolDistrict.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSchoolDistrict <- function(SchoolDistrictID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Enrollment", objectName = "SchoolDistrict", objectId = SchoolDistrictID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SchoolDistrict
	#'
	#' This function creates a SchoolDistrict
	#' @param fieldNames The field values to give the created SchoolDistrict. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A newly created SchoolDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSchoolDistrict <- function(Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Enrollment", objectName = "SchoolDistrict", body = list(DataObject = body), searchFields = append("SchoolDistrictID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SchoolDistrict
	#'
	#' This function modifies a SchoolDistrict
	#' @param fieldNames The field values to give the modified SchoolDistrict. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The modified SchoolDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySchoolDistrict <- function(SchoolDistrictID, Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Enrollment", objectName = "SchoolDistrict", objectId = SchoolDistrictID, body = list(DataObject = body), searchFields = append("SchoolDistrictID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SchoolPathStudents
	#'
	#' This function returns a dataframe or json object of SchoolPathStudents
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SchoolPathStudents. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SchoolPathStudents.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SchoolPathStudent') to get more field paths.
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
	#' @concept Enrollment
	#' @return A list of SchoolPathStudents
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSchoolPathStudents <- function(searchConditionsList = NULL, CreatedTime = F, DistrictID = F, ModifiedTime = F, SchoolPathID = F, SchoolPathStudentID = F, SchoolYearID = F, StudentID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "SchoolPathStudent", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SchoolPathStudent
	#'
	#' This function returns a dataframe or json object of a SchoolPathStudent
	#' @param SchoolPathStudentID The ID of the SchoolPathStudent to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SchoolPathStudent. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SchoolPathStudent.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SchoolPathStudent') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A dataframe or of SchoolPathStudent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSchoolPathStudent <- function(SchoolPathStudentID, CreatedTime = F, DistrictID = F, ModifiedTime = F, SchoolPathID = F, SchoolYearID = F, StudentID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SchoolPathStudentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Enrollment", objectName = "SchoolPathStudent", objectId = SchoolPathStudentID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SchoolPathStudent
	#'
	#' This function deletes a SchoolPathStudent
	#' @param SchoolPathStudentID The ID of the SchoolPathStudent to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The SchoolPathStudentID of the deleted SchoolPathStudent.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSchoolPathStudent <- function(SchoolPathStudentID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Enrollment", objectName = "SchoolPathStudent", objectId = SchoolPathStudentID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SchoolPathStudent
	#'
	#' This function creates a SchoolPathStudent
	#' @param fieldNames The field values to give the created SchoolPathStudent. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A newly created SchoolPathStudent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSchoolPathStudent <- function(DistrictID = NULL, SchoolPathID = NULL, SchoolYearID = NULL, StudentID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Enrollment", objectName = "SchoolPathStudent", body = list(DataObject = body), searchFields = append("SchoolPathStudentID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SchoolPathStudent
	#'
	#' This function modifies a SchoolPathStudent
	#' @param fieldNames The field values to give the modified SchoolPathStudent. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The modified SchoolPathStudent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySchoolPathStudent <- function(SchoolPathStudentID, DistrictID = NULL, SchoolPathID = NULL, SchoolYearID = NULL, StudentID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Enrollment", objectName = "SchoolPathStudent", objectId = SchoolPathStudentID, body = list(DataObject = body), searchFields = append("SchoolPathStudentID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SchoolPaths
	#'
	#' This function returns a dataframe or json object of SchoolPaths
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SchoolPaths. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SchoolPaths.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SchoolPath') to get more field paths.
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
	#' @concept Enrollment
	#' @return A list of SchoolPaths
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSchoolPaths <- function(searchConditionsList = NULL, CreatedTime = F, DistrictID = F, ModifiedTime = F, Name = F, SchoolPathID = F, SchoolPathIDClonedFrom = F, SchoolPathIDClonedTo = F, SchoolPathTypeID = F, SchoolYearID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "SchoolPath", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SchoolPath
	#'
	#' This function returns a dataframe or json object of a SchoolPath
	#' @param SchoolPathID The ID of the SchoolPath to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SchoolPath. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SchoolPath.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SchoolPath') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A dataframe or of SchoolPath
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSchoolPath <- function(SchoolPathID, CreatedTime = F, DistrictID = F, ModifiedTime = F, Name = F, SchoolPathIDClonedFrom = F, SchoolPathIDClonedTo = F, SchoolPathTypeID = F, SchoolYearID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SchoolPathID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Enrollment", objectName = "SchoolPath", objectId = SchoolPathID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SchoolPath
	#'
	#' This function deletes a SchoolPath
	#' @param SchoolPathID The ID of the SchoolPath to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The SchoolPathID of the deleted SchoolPath.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSchoolPath <- function(SchoolPathID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Enrollment", objectName = "SchoolPath", objectId = SchoolPathID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SchoolPath
	#'
	#' This function creates a SchoolPath
	#' @param fieldNames The field values to give the created SchoolPath. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A newly created SchoolPath
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSchoolPath <- function(DistrictID = NULL, Name = NULL, SchoolPathIDClonedFrom = NULL, SchoolPathTypeID = NULL, SchoolYearID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Enrollment", objectName = "SchoolPath", body = list(DataObject = body), searchFields = append("SchoolPathID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SchoolPath
	#'
	#' This function modifies a SchoolPath
	#' @param fieldNames The field values to give the modified SchoolPath. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The modified SchoolPath
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySchoolPath <- function(SchoolPathID, DistrictID = NULL, Name = NULL, SchoolPathIDClonedFrom = NULL, SchoolPathTypeID = NULL, SchoolYearID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Enrollment", objectName = "SchoolPath", objectId = SchoolPathID, body = list(DataObject = body), searchFields = append("SchoolPathID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SchoolPathSchools
	#'
	#' This function returns a dataframe or json object of SchoolPathSchools
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SchoolPathSchools. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SchoolPathSchools.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SchoolPathSchool') to get more field paths.
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
	#' @concept Enrollment
	#' @return A list of SchoolPathSchools
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSchoolPathSchools <- function(searchConditionsList = NULL, CodeDescription = F, CreatedTime = F, DistrictID = F, IsOverriddenForStudent = F, ModifiedTime = F, Order = F, OverriddenSchoolName = F, SchoolID = F, SchoolPathID = F, SchoolPathSchoolID = F, SchoolPathSchoolIDClonedFrom = F, SchoolPathSchoolIDClonedTo = F, SchoolYearID = F, StudentHasPermit = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "SchoolPathSchool", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SchoolPathSchool
	#'
	#' This function returns a dataframe or json object of a SchoolPathSchool
	#' @param SchoolPathSchoolID The ID of the SchoolPathSchool to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SchoolPathSchool. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SchoolPathSchool.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SchoolPathSchool') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A dataframe or of SchoolPathSchool
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSchoolPathSchool <- function(SchoolPathSchoolID, CodeDescription = F, CreatedTime = F, DistrictID = F, IsOverriddenForStudent = F, ModifiedTime = F, Order = F, OverriddenSchoolName = F, SchoolID = F, SchoolPathID = F, SchoolPathSchoolIDClonedFrom = F, SchoolPathSchoolIDClonedTo = F, SchoolYearID = F, StudentHasPermit = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SchoolPathSchoolID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Enrollment", objectName = "SchoolPathSchool", objectId = SchoolPathSchoolID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SchoolPathSchool
	#'
	#' This function deletes a SchoolPathSchool
	#' @param SchoolPathSchoolID The ID of the SchoolPathSchool to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The SchoolPathSchoolID of the deleted SchoolPathSchool.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSchoolPathSchool <- function(SchoolPathSchoolID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Enrollment", objectName = "SchoolPathSchool", objectId = SchoolPathSchoolID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SchoolPathSchool
	#'
	#' This function creates a SchoolPathSchool
	#' @param fieldNames The field values to give the created SchoolPathSchool. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A newly created SchoolPathSchool
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSchoolPathSchool <- function(DistrictID = NULL, Order = NULL, SchoolID = NULL, SchoolPathID = NULL, SchoolPathSchoolIDClonedFrom = NULL, SchoolYearID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Enrollment", objectName = "SchoolPathSchool", body = list(DataObject = body), searchFields = append("SchoolPathSchoolID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SchoolPathSchool
	#'
	#' This function modifies a SchoolPathSchool
	#' @param fieldNames The field values to give the modified SchoolPathSchool. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The modified SchoolPathSchool
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySchoolPathSchool <- function(SchoolPathSchoolID, DistrictID = NULL, Order = NULL, SchoolID = NULL, SchoolPathID = NULL, SchoolPathSchoolIDClonedFrom = NULL, SchoolYearID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Enrollment", objectName = "SchoolPathSchool", objectId = SchoolPathSchoolID, body = list(DataObject = body), searchFields = append("SchoolPathSchoolID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SchoolPathTypes
	#'
	#' This function returns a dataframe or json object of SchoolPathTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SchoolPathTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SchoolPathTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SchoolPathType') to get more field paths.
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
	#' @concept Enrollment
	#' @return A list of SchoolPathTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSchoolPathTypes <- function(searchConditionsList = NULL, Code = F, CodeDescription = F, CreatedTime = F, Description = F, ModifiedTime = F, SchoolPathTypeID = F, SkywardHash = F, SkywardID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "SchoolPathType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SchoolPathType
	#'
	#' This function returns a dataframe or json object of a SchoolPathType
	#' @param SchoolPathTypeID The ID of the SchoolPathType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SchoolPathType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SchoolPathType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SchoolPathType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A dataframe or of SchoolPathType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSchoolPathType <- function(SchoolPathTypeID, Code = F, CodeDescription = F, CreatedTime = F, Description = F, ModifiedTime = F, SkywardHash = F, SkywardID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SchoolPathTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Enrollment", objectName = "SchoolPathType", objectId = SchoolPathTypeID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SchoolPathType
	#'
	#' This function deletes a SchoolPathType
	#' @param SchoolPathTypeID The ID of the SchoolPathType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The SchoolPathTypeID of the deleted SchoolPathType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSchoolPathType <- function(SchoolPathTypeID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Enrollment", objectName = "SchoolPathType", objectId = SchoolPathTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SchoolPathType
	#'
	#' This function creates a SchoolPathType
	#' @param fieldNames The field values to give the created SchoolPathType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A newly created SchoolPathType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSchoolPathType <- function(Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Enrollment", objectName = "SchoolPathType", body = list(DataObject = body), searchFields = append("SchoolPathTypeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SchoolPathType
	#'
	#' This function modifies a SchoolPathType
	#' @param fieldNames The field values to give the modified SchoolPathType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The modified SchoolPathType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySchoolPathType <- function(SchoolPathTypeID, Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Enrollment", objectName = "SchoolPathType", objectId = SchoolPathTypeID, body = list(DataObject = body), searchFields = append("SchoolPathTypeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempNoShowEntryWithdrawalErrors
	#'
	#' This function returns a dataframe or json object of TempNoShowEntryWithdrawalErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempNoShowEntryWithdrawalErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempNoShowEntryWithdrawalErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempNoShowEntryWithdrawalError') to get more field paths.
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
	#' @concept Enrollment
	#' @return A list of TempNoShowEntryWithdrawalErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempNoShowEntryWithdrawalErrors <- function(searchConditionsList = NULL, CreatedTime = F, Error = F, ErrorDetail = F, ModifiedTime = F, TempNoShowEntryWithdrawalErrorID = F, TempNoShowEntryWithdrawalID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "TempNoShowEntryWithdrawalError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempNoShowEntryWithdrawalError
	#'
	#' This function returns a dataframe or json object of a TempNoShowEntryWithdrawalError
	#' @param TempNoShowEntryWithdrawalErrorID The ID of the TempNoShowEntryWithdrawalError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempNoShowEntryWithdrawalError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempNoShowEntryWithdrawalError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempNoShowEntryWithdrawalError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A dataframe or of TempNoShowEntryWithdrawalError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempNoShowEntryWithdrawalError <- function(TempNoShowEntryWithdrawalErrorID, CreatedTime = F, Error = F, ErrorDetail = F, ModifiedTime = F, TempNoShowEntryWithdrawalID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempNoShowEntryWithdrawalErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Enrollment", objectName = "TempNoShowEntryWithdrawalError", objectId = TempNoShowEntryWithdrawalErrorID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempNoShowEntryWithdrawalError
	#'
	#' This function deletes a TempNoShowEntryWithdrawalError
	#' @param TempNoShowEntryWithdrawalErrorID The ID of the TempNoShowEntryWithdrawalError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The TempNoShowEntryWithdrawalErrorID of the deleted TempNoShowEntryWithdrawalError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempNoShowEntryWithdrawalError <- function(TempNoShowEntryWithdrawalErrorID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Enrollment", objectName = "TempNoShowEntryWithdrawalError", objectId = TempNoShowEntryWithdrawalErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempNoShowEntryWithdrawalError
	#'
	#' This function creates a TempNoShowEntryWithdrawalError
	#' @param fieldNames The field values to give the created TempNoShowEntryWithdrawalError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return A newly created TempNoShowEntryWithdrawalError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempNoShowEntryWithdrawalError <- function(Error = NULL, ErrorDetail = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Enrollment", objectName = "TempNoShowEntryWithdrawalError", body = list(DataObject = body), searchFields = append("TempNoShowEntryWithdrawalErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempNoShowEntryWithdrawalError
	#'
	#' This function modifies a TempNoShowEntryWithdrawalError
	#' @param fieldNames The field values to give the modified TempNoShowEntryWithdrawalError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Enrollment
	#' @return The modified TempNoShowEntryWithdrawalError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempNoShowEntryWithdrawalError <- function(TempNoShowEntryWithdrawalErrorID, Error = NULL, ErrorDetail = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Enrollment", objectName = "TempNoShowEntryWithdrawalError", objectId = TempNoShowEntryWithdrawalErrorID, body = list(DataObject = body), searchFields = append("TempNoShowEntryWithdrawalErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}
