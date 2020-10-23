
	#' List RubricAssessmentMNS
	#'
	#' This function returns a dataframe or json object of RubricAssessmentMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given RubricAssessmentMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the RubricAssessmentMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('RubricAssessmentMN') to get more field paths.
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
	#' @concept Special Programs
	#' @return A list of RubricAssessmentMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listRubricAssessmentMNS <- function(searchConditionsList = NULL, RubricAssessmentMNID = F, EntityID = F, SchoolYearID = F, CourseID = F, StateStandardCodeMNID = F, MCCCTermImportID = F, ScoreValue = F, RubricLevel = F, StaffID = F, EntryComment = F, ExitComment = F, StartDate = F, EndDate = F, IsActive = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StateAssessmentToolMNID = F, StateImplementationStatusMNID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialPrograms", objectName = "RubricAssessmentMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a RubricAssessmentMN
	#'
	#' This function returns a dataframe or json object of a RubricAssessmentMN
	#' @param RubricAssessmentMNID The ID of the RubricAssessmentMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given RubricAssessmentMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the RubricAssessmentMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('RubricAssessmentMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A dataframe or of RubricAssessmentMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getRubricAssessmentMN <- function(RubricAssessmentMNID, EntityID = F, SchoolYearID = F, CourseID = F, StateStandardCodeMNID = F, MCCCTermImportID = F, ScoreValue = F, RubricLevel = F, StaffID = F, EntryComment = F, ExitComment = F, StartDate = F, EndDate = F, IsActive = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StateAssessmentToolMNID = F, StateImplementationStatusMNID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "RubricAssessmentMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialPrograms", objectName = "RubricAssessmentMN", objectId = RubricAssessmentMNID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a RubricAssessmentMN
	#'
	#' This function deletes a RubricAssessmentMN
	#' @param RubricAssessmentMNID The ID of the RubricAssessmentMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The RubricAssessmentMNID of the deleted RubricAssessmentMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteRubricAssessmentMN <- function(RubricAssessmentMNID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialPrograms", objectName = "RubricAssessmentMN", objectId = RubricAssessmentMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a RubricAssessmentMN
	#'
	#' This function creates a RubricAssessmentMN
	#' @param fieldNames The field values to give the created RubricAssessmentMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A newly created RubricAssessmentMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createRubricAssessmentMN <- function(EntityID = NULL, SchoolYearID = NULL, CourseID = NULL, StateStandardCodeMNID = NULL, MCCCTermImportID = NULL, ScoreValue = NULL, RubricLevel = NULL, StaffID = NULL, EntryComment = NULL, ExitComment = NULL, StartDate = NULL, EndDate = NULL, StudentID = NULL, StateAssessmentToolMNID = NULL, StateImplementationStatusMNID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialPrograms", objectName = "RubricAssessmentMN", body = list(DataObject = body), searchFields = append("RubricAssessmentMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a RubricAssessmentMN
	#'
	#' This function modifies a RubricAssessmentMN
	#' @param fieldNames The field values to give the modified RubricAssessmentMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The modified RubricAssessmentMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyRubricAssessmentMN <- function(RubricAssessmentMNID, EntityID = NULL, SchoolYearID = NULL, CourseID = NULL, StateStandardCodeMNID = NULL, MCCCTermImportID = NULL, ScoreValue = NULL, RubricLevel = NULL, StaffID = NULL, EntryComment = NULL, ExitComment = NULL, StartDate = NULL, EndDate = NULL, StudentID = NULL, StateAssessmentToolMNID = NULL, StateImplementationStatusMNID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialPrograms", objectName = "RubricAssessmentMN", objectId = RubricAssessmentMNID, body = list(DataObject = body), searchFields = append("RubricAssessmentMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AtRiskWithdrawalRecordCriteria
	#'
	#' This function returns a dataframe or json object of AtRiskWithdrawalRecordCriteria
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AtRiskWithdrawalRecordCriteria. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AtRiskWithdrawalRecordCriteria.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AtRiskWithdrawalRecordCriteria') to get more field paths.
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
	#' @concept Special Programs
	#' @return A list of AtRiskWithdrawalRecordCriteria
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAtRiskWithdrawalRecordCriteria <- function(searchConditionsList = NULL, AtRiskWithdrawalRecordCriteriaID = F, WithdrawalDateHigh = F, WithdrawalDateLow = F, WithdrawalCodeID = F, AtRiskIdentificationCriterionID = F, CriteriaLevel = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialPrograms", objectName = "AtRiskWithdrawalRecordCriteria", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AtRiskWithdrawalRecordCriteria
	#'
	#' This function returns a dataframe or json object of an AtRiskWithdrawalRecordCriteria
	#' @param AtRiskWithdrawalRecordCriteriaID The ID of the AtRiskWithdrawalRecordCriteria to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AtRiskWithdrawalRecordCriteria. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AtRiskWithdrawalRecordCriteria.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AtRiskWithdrawalRecordCriteria') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A dataframe or of AtRiskWithdrawalRecordCriteria
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAtRiskWithdrawalRecordCriteria <- function(AtRiskWithdrawalRecordCriteriaID, WithdrawalDateHigh = F, WithdrawalDateLow = F, WithdrawalCodeID = F, AtRiskIdentificationCriterionID = F, CriteriaLevel = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AtRiskWithdrawalRecordCriteriaID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialPrograms", objectName = "AtRiskWithdrawalRecordCriteria", objectId = AtRiskWithdrawalRecordCriteriaID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AtRiskWithdrawalRecordCriteria
	#'
	#' This function deletes an AtRiskWithdrawalRecordCriteria
	#' @param AtRiskWithdrawalRecordCriteriaID The ID of the AtRiskWithdrawalRecordCriteria to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The AtRiskWithdrawalRecordCriteriaID of the deleted AtRiskWithdrawalRecordCriteria.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAtRiskWithdrawalRecordCriteria <- function(AtRiskWithdrawalRecordCriteriaID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialPrograms", objectName = "AtRiskWithdrawalRecordCriteria", objectId = AtRiskWithdrawalRecordCriteriaID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AtRiskWithdrawalRecordCriteria
	#'
	#' This function creates an AtRiskWithdrawalRecordCriteria
	#' @param fieldNames The field values to give the created AtRiskWithdrawalRecordCriteria. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A newly created AtRiskWithdrawalRecordCriteria
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAtRiskWithdrawalRecordCriteria <- function(WithdrawalDateHigh = NULL, WithdrawalDateLow = NULL, WithdrawalCodeID = NULL, AtRiskIdentificationCriterionID = NULL, CriteriaLevel = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialPrograms", objectName = "AtRiskWithdrawalRecordCriteria", body = list(DataObject = body), searchFields = append("AtRiskWithdrawalRecordCriteriaID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AtRiskWithdrawalRecordCriteria
	#'
	#' This function modifies an AtRiskWithdrawalRecordCriteria
	#' @param fieldNames The field values to give the modified AtRiskWithdrawalRecordCriteria. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The modified AtRiskWithdrawalRecordCriteria
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAtRiskWithdrawalRecordCriteria <- function(AtRiskWithdrawalRecordCriteriaID, WithdrawalDateHigh = NULL, WithdrawalDateLow = NULL, WithdrawalCodeID = NULL, AtRiskIdentificationCriterionID = NULL, CriteriaLevel = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialPrograms", objectName = "AtRiskWithdrawalRecordCriteria", objectId = AtRiskWithdrawalRecordCriteriaID, body = list(DataObject = body), searchFields = append("AtRiskWithdrawalRecordCriteriaID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AtRiskIdentificationCriteria
	#'
	#' This function returns a dataframe or json object of AtRiskIdentificationCriteria
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AtRiskIdentificationCriteria. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AtRiskIdentificationCriteria.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AtRiskIdentificationCriterion') to get more field paths.
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
	#' @concept Special Programs
	#' @return A list of AtRiskIdentificationCriteria
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAtRiskIdentificationCriteria <- function(searchConditionsList = NULL, AtRiskIdentificationCriterionID = F, CriterionNumber = F, Code = F, Description = F, IsStateRequirement = F, StateAtRiskCriterionTypeCodeID = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialPrograms", objectName = "AtRiskIdentificationCriterion", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AtRiskIdentificationCriterion
	#'
	#' This function returns a dataframe or json object of an AtRiskIdentificationCriterion
	#' @param AtRiskIdentificationCriterionID The ID of the AtRiskIdentificationCriterion to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AtRiskIdentificationCriterion. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AtRiskIdentificationCriterion.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AtRiskIdentificationCriterion') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A dataframe or of AtRiskIdentificationCriterion
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAtRiskIdentificationCriterion <- function(AtRiskIdentificationCriterionID, CriterionNumber = F, Code = F, Description = F, IsStateRequirement = F, StateAtRiskCriterionTypeCodeID = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AtRiskIdentificationCriterionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialPrograms", objectName = "AtRiskIdentificationCriterion", objectId = AtRiskIdentificationCriterionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AtRiskIdentificationCriterion
	#'
	#' This function deletes an AtRiskIdentificationCriterion
	#' @param AtRiskIdentificationCriterionID The ID of the AtRiskIdentificationCriterion to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The AtRiskIdentificationCriterionID of the deleted AtRiskIdentificationCriterion.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAtRiskIdentificationCriterion <- function(AtRiskIdentificationCriterionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialPrograms", objectName = "AtRiskIdentificationCriterion", objectId = AtRiskIdentificationCriterionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AtRiskIdentificationCriterion
	#'
	#' This function creates an AtRiskIdentificationCriterion
	#' @param fieldNames The field values to give the created AtRiskIdentificationCriterion. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A newly created AtRiskIdentificationCriterion
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAtRiskIdentificationCriterion <- function(CriterionNumber = NULL, Code = NULL, Description = NULL, IsStateRequirement = NULL, StateAtRiskCriterionTypeCodeID = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialPrograms", objectName = "AtRiskIdentificationCriterion", body = list(DataObject = body), searchFields = append("AtRiskIdentificationCriterionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AtRiskIdentificationCriterion
	#'
	#' This function modifies an AtRiskIdentificationCriterion
	#' @param fieldNames The field values to give the modified AtRiskIdentificationCriterion. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The modified AtRiskIdentificationCriterion
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAtRiskIdentificationCriterion <- function(AtRiskIdentificationCriterionID, CriterionNumber = NULL, Code = NULL, Description = NULL, IsStateRequirement = NULL, StateAtRiskCriterionTypeCodeID = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialPrograms", objectName = "AtRiskIdentificationCriterion", objectId = AtRiskIdentificationCriterionID, body = list(DataObject = body), searchFields = append("AtRiskIdentificationCriterionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List GiftedTalentedAreas
	#'
	#' This function returns a dataframe or json object of GiftedTalentedAreas
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given GiftedTalentedAreas. Defaults to FALSE for all return fields which, for convenience, returns all fields for the GiftedTalentedAreas.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('GiftedTalentedArea') to get more field paths.
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
	#' @concept Special Programs
	#' @return A list of GiftedTalentedAreas
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listGiftedTalentedAreas <- function(searchConditionsList = NULL, GiftedTalentedAreaID = F, DistrictID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialPrograms", objectName = "GiftedTalentedArea", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a GiftedTalentedArea
	#'
	#' This function returns a dataframe or json object of a GiftedTalentedArea
	#' @param GiftedTalentedAreaID The ID of the GiftedTalentedArea to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given GiftedTalentedArea. Defaults to FALSE for all return fields which, for convenience, returns all fields for the GiftedTalentedArea.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('GiftedTalentedArea') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A dataframe or of GiftedTalentedArea
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getGiftedTalentedArea <- function(GiftedTalentedAreaID, DistrictID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "GiftedTalentedAreaID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialPrograms", objectName = "GiftedTalentedArea", objectId = GiftedTalentedAreaID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a GiftedTalentedArea
	#'
	#' This function deletes a GiftedTalentedArea
	#' @param GiftedTalentedAreaID The ID of the GiftedTalentedArea to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The GiftedTalentedAreaID of the deleted GiftedTalentedArea.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteGiftedTalentedArea <- function(GiftedTalentedAreaID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialPrograms", objectName = "GiftedTalentedArea", objectId = GiftedTalentedAreaID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a GiftedTalentedArea
	#'
	#' This function creates a GiftedTalentedArea
	#' @param fieldNames The field values to give the created GiftedTalentedArea. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A newly created GiftedTalentedArea
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createGiftedTalentedArea <- function(DistrictID = NULL, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialPrograms", objectName = "GiftedTalentedArea", body = list(DataObject = body), searchFields = append("GiftedTalentedAreaID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a GiftedTalentedArea
	#'
	#' This function modifies a GiftedTalentedArea
	#' @param fieldNames The field values to give the modified GiftedTalentedArea. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The modified GiftedTalentedArea
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyGiftedTalentedArea <- function(GiftedTalentedAreaID, DistrictID = NULL, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialPrograms", objectName = "GiftedTalentedArea", objectId = GiftedTalentedAreaID, body = list(DataObject = body), searchFields = append("GiftedTalentedAreaID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List GiftedAndTalentedMNS
	#'
	#' This function returns a dataframe or json object of GiftedAndTalentedMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given GiftedAndTalentedMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the GiftedAndTalentedMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('GiftedAndTalentedMN') to get more field paths.
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
	#' @concept Special Programs
	#' @return A list of GiftedAndTalentedMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listGiftedAndTalentedMNS <- function(searchConditionsList = NULL, GiftedAndTalentedMNID = F, DistrictID = F, StartDate = F, EndDate = F, IsActive = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialPrograms", objectName = "GiftedAndTalentedMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a GiftedAndTalentedMN
	#'
	#' This function returns a dataframe or json object of a GiftedAndTalentedMN
	#' @param GiftedAndTalentedMNID The ID of the GiftedAndTalentedMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given GiftedAndTalentedMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the GiftedAndTalentedMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('GiftedAndTalentedMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A dataframe or of GiftedAndTalentedMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getGiftedAndTalentedMN <- function(GiftedAndTalentedMNID, DistrictID = F, StartDate = F, EndDate = F, IsActive = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "GiftedAndTalentedMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialPrograms", objectName = "GiftedAndTalentedMN", objectId = GiftedAndTalentedMNID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a GiftedAndTalentedMN
	#'
	#' This function deletes a GiftedAndTalentedMN
	#' @param GiftedAndTalentedMNID The ID of the GiftedAndTalentedMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The GiftedAndTalentedMNID of the deleted GiftedAndTalentedMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteGiftedAndTalentedMN <- function(GiftedAndTalentedMNID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialPrograms", objectName = "GiftedAndTalentedMN", objectId = GiftedAndTalentedMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a GiftedAndTalentedMN
	#'
	#' This function creates a GiftedAndTalentedMN
	#' @param fieldNames The field values to give the created GiftedAndTalentedMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A newly created GiftedAndTalentedMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createGiftedAndTalentedMN <- function(DistrictID = NULL, StartDate = NULL, EndDate = NULL, StudentID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialPrograms", objectName = "GiftedAndTalentedMN", body = list(DataObject = body), searchFields = append("GiftedAndTalentedMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a GiftedAndTalentedMN
	#'
	#' This function modifies a GiftedAndTalentedMN
	#' @param fieldNames The field values to give the modified GiftedAndTalentedMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The modified GiftedAndTalentedMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyGiftedAndTalentedMN <- function(GiftedAndTalentedMNID, DistrictID = NULL, StartDate = NULL, EndDate = NULL, StudentID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialPrograms", objectName = "GiftedAndTalentedMN", objectId = GiftedAndTalentedMNID, body = list(DataObject = body), searchFields = append("GiftedAndTalentedMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List HomeboundHospitalMNS
	#'
	#' This function returns a dataframe or json object of HomeboundHospitalMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given HomeboundHospitalMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the HomeboundHospitalMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('HomeboundHospitalMN') to get more field paths.
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
	#' @concept Special Programs
	#' @return A list of HomeboundHospitalMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listHomeboundHospitalMNS <- function(searchConditionsList = NULL, HomeboundHospitalMNID = F, DistrictID = F, EntryComment = F, ExitComment = F, StartDate = F, EndDate = F, IsActive = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialPrograms", objectName = "HomeboundHospitalMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a HomeboundHospitalMN
	#'
	#' This function returns a dataframe or json object of a HomeboundHospitalMN
	#' @param HomeboundHospitalMNID The ID of the HomeboundHospitalMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given HomeboundHospitalMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the HomeboundHospitalMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('HomeboundHospitalMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A dataframe or of HomeboundHospitalMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getHomeboundHospitalMN <- function(HomeboundHospitalMNID, DistrictID = F, EntryComment = F, ExitComment = F, StartDate = F, EndDate = F, IsActive = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "HomeboundHospitalMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialPrograms", objectName = "HomeboundHospitalMN", objectId = HomeboundHospitalMNID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a HomeboundHospitalMN
	#'
	#' This function deletes a HomeboundHospitalMN
	#' @param HomeboundHospitalMNID The ID of the HomeboundHospitalMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The HomeboundHospitalMNID of the deleted HomeboundHospitalMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteHomeboundHospitalMN <- function(HomeboundHospitalMNID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialPrograms", objectName = "HomeboundHospitalMN", objectId = HomeboundHospitalMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a HomeboundHospitalMN
	#'
	#' This function creates a HomeboundHospitalMN
	#' @param fieldNames The field values to give the created HomeboundHospitalMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A newly created HomeboundHospitalMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createHomeboundHospitalMN <- function(DistrictID = NULL, EntryComment = NULL, ExitComment = NULL, StartDate = NULL, EndDate = NULL, StudentID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialPrograms", objectName = "HomeboundHospitalMN", body = list(DataObject = body), searchFields = append("HomeboundHospitalMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a HomeboundHospitalMN
	#'
	#' This function modifies a HomeboundHospitalMN
	#' @param fieldNames The field values to give the modified HomeboundHospitalMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The modified HomeboundHospitalMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyHomeboundHospitalMN <- function(HomeboundHospitalMNID, DistrictID = NULL, EntryComment = NULL, ExitComment = NULL, StartDate = NULL, EndDate = NULL, StudentID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialPrograms", objectName = "HomeboundHospitalMN", objectId = HomeboundHospitalMNID, body = list(DataObject = body), searchFields = append("HomeboundHospitalMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List LimitedEnglishProficiencyMNS
	#'
	#' This function returns a dataframe or json object of LimitedEnglishProficiencyMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given LimitedEnglishProficiencyMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the LimitedEnglishProficiencyMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('LimitedEnglishProficiencyMN') to get more field paths.
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
	#' @concept Special Programs
	#' @return A list of LimitedEnglishProficiencyMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listLimitedEnglishProficiencyMNS <- function(searchConditionsList = NULL, LimitedEnglishProficiencyMNID = F, DistrictID = F, EntryComment = F, ExitComment = F, IsReceivingServices = F, EnglishLanguageProficiencyLevel = F, StartDate = F, EndDate = F, IsActive = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsBenefitingFromNCLBTitleIIIAllocation = F, InstructionalProgram = F, InstrumentUsed = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialPrograms", objectName = "LimitedEnglishProficiencyMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a LimitedEnglishProficiencyMN
	#'
	#' This function returns a dataframe or json object of a LimitedEnglishProficiencyMN
	#' @param LimitedEnglishProficiencyMNID The ID of the LimitedEnglishProficiencyMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given LimitedEnglishProficiencyMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the LimitedEnglishProficiencyMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('LimitedEnglishProficiencyMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A dataframe or of LimitedEnglishProficiencyMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getLimitedEnglishProficiencyMN <- function(LimitedEnglishProficiencyMNID, DistrictID = F, EntryComment = F, ExitComment = F, IsReceivingServices = F, EnglishLanguageProficiencyLevel = F, StartDate = F, EndDate = F, IsActive = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsBenefitingFromNCLBTitleIIIAllocation = F, InstructionalProgram = F, InstrumentUsed = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "LimitedEnglishProficiencyMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialPrograms", objectName = "LimitedEnglishProficiencyMN", objectId = LimitedEnglishProficiencyMNID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a LimitedEnglishProficiencyMN
	#'
	#' This function deletes a LimitedEnglishProficiencyMN
	#' @param LimitedEnglishProficiencyMNID The ID of the LimitedEnglishProficiencyMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The LimitedEnglishProficiencyMNID of the deleted LimitedEnglishProficiencyMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteLimitedEnglishProficiencyMN <- function(LimitedEnglishProficiencyMNID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialPrograms", objectName = "LimitedEnglishProficiencyMN", objectId = LimitedEnglishProficiencyMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a LimitedEnglishProficiencyMN
	#'
	#' This function creates a LimitedEnglishProficiencyMN
	#' @param fieldNames The field values to give the created LimitedEnglishProficiencyMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A newly created LimitedEnglishProficiencyMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createLimitedEnglishProficiencyMN <- function(DistrictID = NULL, EntryComment = NULL, ExitComment = NULL, IsReceivingServices = NULL, EnglishLanguageProficiencyLevel = NULL, StartDate = NULL, EndDate = NULL, StudentID = NULL, IsBenefitingFromNCLBTitleIIIAllocation = NULL, InstructionalProgram = NULL, InstrumentUsed = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialPrograms", objectName = "LimitedEnglishProficiencyMN", body = list(DataObject = body), searchFields = append("LimitedEnglishProficiencyMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a LimitedEnglishProficiencyMN
	#'
	#' This function modifies a LimitedEnglishProficiencyMN
	#' @param fieldNames The field values to give the modified LimitedEnglishProficiencyMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The modified LimitedEnglishProficiencyMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyLimitedEnglishProficiencyMN <- function(LimitedEnglishProficiencyMNID, DistrictID = NULL, EntryComment = NULL, ExitComment = NULL, IsReceivingServices = NULL, EnglishLanguageProficiencyLevel = NULL, StartDate = NULL, EndDate = NULL, StudentID = NULL, IsBenefitingFromNCLBTitleIIIAllocation = NULL, InstructionalProgram = NULL, InstrumentUsed = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialPrograms", objectName = "LimitedEnglishProficiencyMN", objectId = LimitedEnglishProficiencyMNID, body = list(DataObject = body), searchFields = append("LimitedEnglishProficiencyMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List OptOutMNS
	#'
	#' This function returns a dataframe or json object of OptOutMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OptOutMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OptOutMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OptOutMN') to get more field paths.
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
	#' @concept Special Programs
	#' @return A list of OptOutMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listOptOutMNS <- function(searchConditionsList = NULL, OptOutMNID = F, DistrictID = F, EntryComment = F, ExitComment = F, IsMinnesotaHealthCareOptOut = F, StartDate = F, EndDate = F, IsActive = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialPrograms", objectName = "OptOutMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an OptOutMN
	#'
	#' This function returns a dataframe or json object of an OptOutMN
	#' @param OptOutMNID The ID of the OptOutMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OptOutMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OptOutMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OptOutMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A dataframe or of OptOutMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getOptOutMN <- function(OptOutMNID, DistrictID = F, EntryComment = F, ExitComment = F, IsMinnesotaHealthCareOptOut = F, StartDate = F, EndDate = F, IsActive = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "OptOutMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialPrograms", objectName = "OptOutMN", objectId = OptOutMNID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an OptOutMN
	#'
	#' This function deletes an OptOutMN
	#' @param OptOutMNID The ID of the OptOutMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The OptOutMNID of the deleted OptOutMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteOptOutMN <- function(OptOutMNID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialPrograms", objectName = "OptOutMN", objectId = OptOutMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an OptOutMN
	#'
	#' This function creates an OptOutMN
	#' @param fieldNames The field values to give the created OptOutMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A newly created OptOutMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createOptOutMN <- function(DistrictID = NULL, EntryComment = NULL, ExitComment = NULL, IsMinnesotaHealthCareOptOut = NULL, StartDate = NULL, EndDate = NULL, StudentID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialPrograms", objectName = "OptOutMN", body = list(DataObject = body), searchFields = append("OptOutMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an OptOutMN
	#'
	#' This function modifies an OptOutMN
	#' @param fieldNames The field values to give the modified OptOutMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The modified OptOutMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyOptOutMN <- function(OptOutMNID, DistrictID = NULL, EntryComment = NULL, ExitComment = NULL, IsMinnesotaHealthCareOptOut = NULL, StartDate = NULL, EndDate = NULL, StudentID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialPrograms", objectName = "OptOutMN", objectId = OptOutMNID, body = list(DataObject = body), searchFields = append("OptOutMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SpecialPupilsCareAndTreatmentMNS
	#'
	#' This function returns a dataframe or json object of SpecialPupilsCareAndTreatmentMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SpecialPupilsCareAndTreatmentMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SpecialPupilsCareAndTreatmentMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SpecialPupilsCareAndTreatmentMN') to get more field paths.
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
	#' @concept Special Programs
	#' @return A list of SpecialPupilsCareAndTreatmentMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSpecialPupilsCareAndTreatmentMNS <- function(searchConditionsList = NULL, SpecialPupilsCareAndTreatmentMNID = F, DistrictID = F, EntryComment = F, ExitComment = F, StartDate = F, EndDate = F, IsActive = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialPrograms", objectName = "SpecialPupilsCareAndTreatmentMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SpecialPupilsCareAndTreatmentMN
	#'
	#' This function returns a dataframe or json object of a SpecialPupilsCareAndTreatmentMN
	#' @param SpecialPupilsCareAndTreatmentMNID The ID of the SpecialPupilsCareAndTreatmentMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SpecialPupilsCareAndTreatmentMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SpecialPupilsCareAndTreatmentMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SpecialPupilsCareAndTreatmentMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A dataframe or of SpecialPupilsCareAndTreatmentMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSpecialPupilsCareAndTreatmentMN <- function(SpecialPupilsCareAndTreatmentMNID, DistrictID = F, EntryComment = F, ExitComment = F, StartDate = F, EndDate = F, IsActive = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SpecialPupilsCareAndTreatmentMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialPrograms", objectName = "SpecialPupilsCareAndTreatmentMN", objectId = SpecialPupilsCareAndTreatmentMNID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SpecialPupilsCareAndTreatmentMN
	#'
	#' This function deletes a SpecialPupilsCareAndTreatmentMN
	#' @param SpecialPupilsCareAndTreatmentMNID The ID of the SpecialPupilsCareAndTreatmentMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The SpecialPupilsCareAndTreatmentMNID of the deleted SpecialPupilsCareAndTreatmentMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSpecialPupilsCareAndTreatmentMN <- function(SpecialPupilsCareAndTreatmentMNID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialPrograms", objectName = "SpecialPupilsCareAndTreatmentMN", objectId = SpecialPupilsCareAndTreatmentMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SpecialPupilsCareAndTreatmentMN
	#'
	#' This function creates a SpecialPupilsCareAndTreatmentMN
	#' @param fieldNames The field values to give the created SpecialPupilsCareAndTreatmentMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A newly created SpecialPupilsCareAndTreatmentMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSpecialPupilsCareAndTreatmentMN <- function(DistrictID = NULL, EntryComment = NULL, ExitComment = NULL, StartDate = NULL, EndDate = NULL, StudentID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialPrograms", objectName = "SpecialPupilsCareAndTreatmentMN", body = list(DataObject = body), searchFields = append("SpecialPupilsCareAndTreatmentMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SpecialPupilsCareAndTreatmentMN
	#'
	#' This function modifies a SpecialPupilsCareAndTreatmentMN
	#' @param fieldNames The field values to give the modified SpecialPupilsCareAndTreatmentMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The modified SpecialPupilsCareAndTreatmentMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySpecialPupilsCareAndTreatmentMN <- function(SpecialPupilsCareAndTreatmentMNID, DistrictID = NULL, EntryComment = NULL, ExitComment = NULL, StartDate = NULL, EndDate = NULL, StudentID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialPrograms", objectName = "SpecialPupilsCareAndTreatmentMN", objectId = SpecialPupilsCareAndTreatmentMNID, body = list(DataObject = body), searchFields = append("SpecialPupilsCareAndTreatmentMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List HomelessMNS
	#'
	#' This function returns a dataframe or json object of HomelessMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given HomelessMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the HomelessMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('HomelessMN') to get more field paths.
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
	#' @concept Special Programs
	#' @return A list of HomelessMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listHomelessMNS <- function(searchConditionsList = NULL, HomelessMNID = F, DistrictID = F, EntryComment = F, ExitComment = F, StartDate = F, EndDate = F, IsActive = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsMcKinneyVento = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialPrograms", objectName = "HomelessMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a HomelessMN
	#'
	#' This function returns a dataframe or json object of a HomelessMN
	#' @param HomelessMNID The ID of the HomelessMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given HomelessMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the HomelessMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('HomelessMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A dataframe or of HomelessMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getHomelessMN <- function(HomelessMNID, DistrictID = F, EntryComment = F, ExitComment = F, StartDate = F, EndDate = F, IsActive = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsMcKinneyVento = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "HomelessMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialPrograms", objectName = "HomelessMN", objectId = HomelessMNID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a HomelessMN
	#'
	#' This function deletes a HomelessMN
	#' @param HomelessMNID The ID of the HomelessMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The HomelessMNID of the deleted HomelessMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteHomelessMN <- function(HomelessMNID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialPrograms", objectName = "HomelessMN", objectId = HomelessMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a HomelessMN
	#'
	#' This function creates a HomelessMN
	#' @param fieldNames The field values to give the created HomelessMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A newly created HomelessMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createHomelessMN <- function(DistrictID = NULL, EntryComment = NULL, ExitComment = NULL, StartDate = NULL, EndDate = NULL, StudentID = NULL, IsMcKinneyVento = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialPrograms", objectName = "HomelessMN", body = list(DataObject = body), searchFields = append("HomelessMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a HomelessMN
	#'
	#' This function modifies a HomelessMN
	#' @param fieldNames The field values to give the modified HomelessMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The modified HomelessMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyHomelessMN <- function(HomelessMNID, DistrictID = NULL, EntryComment = NULL, ExitComment = NULL, StartDate = NULL, EndDate = NULL, StudentID = NULL, IsMcKinneyVento = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialPrograms", objectName = "HomelessMN", objectId = HomelessMNID, body = list(DataObject = body), searchFields = append("HomelessMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TitleIMNS
	#'
	#' This function returns a dataframe or json object of TitleIMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TitleIMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TitleIMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TitleIMN') to get more field paths.
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
	#' @concept Special Programs
	#' @return A list of TitleIMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTitleIMNS <- function(searchConditionsList = NULL, TitleIMNID = F, EntityID = F, EntryComment = F, ExitComment = F, StartDate = F, EndDate = F, IsActive = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialPrograms", objectName = "TitleIMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TitleIMN
	#'
	#' This function returns a dataframe or json object of a TitleIMN
	#' @param TitleIMNID The ID of the TitleIMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TitleIMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TitleIMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TitleIMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A dataframe or of TitleIMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTitleIMN <- function(TitleIMNID, EntityID = F, EntryComment = F, ExitComment = F, StartDate = F, EndDate = F, IsActive = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TitleIMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialPrograms", objectName = "TitleIMN", objectId = TitleIMNID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TitleIMN
	#'
	#' This function deletes a TitleIMN
	#' @param TitleIMNID The ID of the TitleIMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The TitleIMNID of the deleted TitleIMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTitleIMN <- function(TitleIMNID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialPrograms", objectName = "TitleIMN", objectId = TitleIMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TitleIMN
	#'
	#' This function creates a TitleIMN
	#' @param fieldNames The field values to give the created TitleIMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A newly created TitleIMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTitleIMN <- function(EntityID = NULL, EntryComment = NULL, ExitComment = NULL, StartDate = NULL, EndDate = NULL, StudentID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialPrograms", objectName = "TitleIMN", body = list(DataObject = body), searchFields = append("TitleIMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TitleIMN
	#'
	#' This function modifies a TitleIMN
	#' @param fieldNames The field values to give the modified TitleIMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The modified TitleIMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTitleIMN <- function(TitleIMNID, EntityID = NULL, EntryComment = NULL, ExitComment = NULL, StartDate = NULL, EndDate = NULL, StudentID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialPrograms", objectName = "TitleIMN", objectId = TitleIMNID, body = list(DataObject = body), searchFields = append("TitleIMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Section504Accommodations
	#'
	#' This function returns a dataframe or json object of Section504Accommodations
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Section504Accommodations. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Section504Accommodations.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Section504Accommodation') to get more field paths.
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
	#' @concept Special Programs
	#' @return A list of Section504Accommodations
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSection504Accommodations <- function(searchConditionsList = NULL, Section504AccommodationID = F, DistrictID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialPrograms", objectName = "Section504Accommodation", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Section504Accommodation
	#'
	#' This function returns a dataframe or json object of a Section504Accommodation
	#' @param Section504AccommodationID The ID of the Section504Accommodation to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Section504Accommodation. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Section504Accommodation.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Section504Accommodation') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A dataframe or of Section504Accommodation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSection504Accommodation <- function(Section504AccommodationID, DistrictID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "Section504AccommodationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialPrograms", objectName = "Section504Accommodation", objectId = Section504AccommodationID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Section504Accommodation
	#'
	#' This function deletes a Section504Accommodation
	#' @param Section504AccommodationID The ID of the Section504Accommodation to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The Section504AccommodationID of the deleted Section504Accommodation.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSection504Accommodation <- function(Section504AccommodationID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialPrograms", objectName = "Section504Accommodation", objectId = Section504AccommodationID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Section504Accommodation
	#'
	#' This function creates a Section504Accommodation
	#' @param fieldNames The field values to give the created Section504Accommodation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A newly created Section504Accommodation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSection504Accommodation <- function(DistrictID = NULL, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialPrograms", objectName = "Section504Accommodation", body = list(DataObject = body), searchFields = append("Section504AccommodationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Section504Accommodation
	#'
	#' This function modifies a Section504Accommodation
	#' @param fieldNames The field values to give the modified Section504Accommodation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The modified Section504Accommodation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySection504Accommodation <- function(Section504AccommodationID, DistrictID = NULL, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialPrograms", objectName = "Section504Accommodation", objectId = Section504AccommodationID, body = list(DataObject = body), searchFields = append("Section504AccommodationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Section504EvaluationStatuses
	#'
	#' This function returns a dataframe or json object of Section504EvaluationStatuses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Section504EvaluationStatuses. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Section504EvaluationStatuses.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Section504EvaluationStatus') to get more field paths.
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
	#' @concept Special Programs
	#' @return A list of Section504EvaluationStatuses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSection504EvaluationStatuses <- function(searchConditionsList = NULL, Section504EvaluationStatusID = F, DistrictID = F, IsIneligible = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialPrograms", objectName = "Section504EvaluationStatus", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Section504EvaluationStatus
	#'
	#' This function returns a dataframe or json object of a Section504EvaluationStatus
	#' @param Section504EvaluationStatusID The ID of the Section504EvaluationStatus to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Section504EvaluationStatus. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Section504EvaluationStatus.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Section504EvaluationStatus') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A dataframe or of Section504EvaluationStatus
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSection504EvaluationStatus <- function(Section504EvaluationStatusID, DistrictID = F, IsIneligible = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "Section504EvaluationStatusID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialPrograms", objectName = "Section504EvaluationStatus", objectId = Section504EvaluationStatusID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Section504EvaluationStatus
	#'
	#' This function deletes a Section504EvaluationStatus
	#' @param Section504EvaluationStatusID The ID of the Section504EvaluationStatus to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The Section504EvaluationStatusID of the deleted Section504EvaluationStatus.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSection504EvaluationStatus <- function(Section504EvaluationStatusID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialPrograms", objectName = "Section504EvaluationStatus", objectId = Section504EvaluationStatusID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Section504EvaluationStatus
	#'
	#' This function creates a Section504EvaluationStatus
	#' @param fieldNames The field values to give the created Section504EvaluationStatus. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A newly created Section504EvaluationStatus
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSection504EvaluationStatus <- function(DistrictID = NULL, IsIneligible = NULL, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialPrograms", objectName = "Section504EvaluationStatus", body = list(DataObject = body), searchFields = append("Section504EvaluationStatusID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Section504EvaluationStatus
	#'
	#' This function modifies a Section504EvaluationStatus
	#' @param fieldNames The field values to give the modified Section504EvaluationStatus. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The modified Section504EvaluationStatus
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySection504EvaluationStatus <- function(Section504EvaluationStatusID, DistrictID = NULL, IsIneligible = NULL, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialPrograms", objectName = "Section504EvaluationStatus", objectId = Section504EvaluationStatusID, body = list(DataObject = body), searchFields = append("Section504EvaluationStatusID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Section504ImpairmentCodes
	#'
	#' This function returns a dataframe or json object of Section504ImpairmentCodes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Section504ImpairmentCodes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Section504ImpairmentCodes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Section504ImpairmentCode') to get more field paths.
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
	#' @concept Special Programs
	#' @return A list of Section504ImpairmentCodes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSection504ImpairmentCodes <- function(searchConditionsList = NULL, Section504ImpairmentCodeID = F, DistrictID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialPrograms", objectName = "Section504ImpairmentCode", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Section504ImpairmentCode
	#'
	#' This function returns a dataframe or json object of a Section504ImpairmentCode
	#' @param Section504ImpairmentCodeID The ID of the Section504ImpairmentCode to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Section504ImpairmentCode. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Section504ImpairmentCode.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Section504ImpairmentCode') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A dataframe or of Section504ImpairmentCode
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSection504ImpairmentCode <- function(Section504ImpairmentCodeID, DistrictID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "Section504ImpairmentCodeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialPrograms", objectName = "Section504ImpairmentCode", objectId = Section504ImpairmentCodeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Section504ImpairmentCode
	#'
	#' This function deletes a Section504ImpairmentCode
	#' @param Section504ImpairmentCodeID The ID of the Section504ImpairmentCode to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The Section504ImpairmentCodeID of the deleted Section504ImpairmentCode.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSection504ImpairmentCode <- function(Section504ImpairmentCodeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialPrograms", objectName = "Section504ImpairmentCode", objectId = Section504ImpairmentCodeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Section504ImpairmentCode
	#'
	#' This function creates a Section504ImpairmentCode
	#' @param fieldNames The field values to give the created Section504ImpairmentCode. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A newly created Section504ImpairmentCode
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSection504ImpairmentCode <- function(DistrictID = NULL, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialPrograms", objectName = "Section504ImpairmentCode", body = list(DataObject = body), searchFields = append("Section504ImpairmentCodeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Section504ImpairmentCode
	#'
	#' This function modifies a Section504ImpairmentCode
	#' @param fieldNames The field values to give the modified Section504ImpairmentCode. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The modified Section504ImpairmentCode
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySection504ImpairmentCode <- function(Section504ImpairmentCodeID, DistrictID = NULL, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialPrograms", objectName = "Section504ImpairmentCode", objectId = Section504ImpairmentCodeID, body = list(DataObject = body), searchFields = append("Section504ImpairmentCodeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StateAtRiskCriterionTypeCodes
	#'
	#' This function returns a dataframe or json object of StateAtRiskCriterionTypeCodes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StateAtRiskCriterionTypeCodes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StateAtRiskCriterionTypeCodes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StateAtRiskCriterionTypeCode') to get more field paths.
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
	#' @concept Special Programs
	#' @return A list of StateAtRiskCriterionTypeCodes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStateAtRiskCriterionTypeCodes <- function(searchConditionsList = NULL, StateAtRiskCriterionTypeCodeID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialPrograms", objectName = "StateAtRiskCriterionTypeCode", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StateAtRiskCriterionTypeCode
	#'
	#' This function returns a dataframe or json object of a StateAtRiskCriterionTypeCode
	#' @param StateAtRiskCriterionTypeCodeID The ID of the StateAtRiskCriterionTypeCode to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StateAtRiskCriterionTypeCode. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StateAtRiskCriterionTypeCode.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StateAtRiskCriterionTypeCode') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A dataframe or of StateAtRiskCriterionTypeCode
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStateAtRiskCriterionTypeCode <- function(StateAtRiskCriterionTypeCodeID, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StateAtRiskCriterionTypeCodeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialPrograms", objectName = "StateAtRiskCriterionTypeCode", objectId = StateAtRiskCriterionTypeCodeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StateAtRiskCriterionTypeCode
	#'
	#' This function deletes a StateAtRiskCriterionTypeCode
	#' @param StateAtRiskCriterionTypeCodeID The ID of the StateAtRiskCriterionTypeCode to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The StateAtRiskCriterionTypeCodeID of the deleted StateAtRiskCriterionTypeCode.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStateAtRiskCriterionTypeCode <- function(StateAtRiskCriterionTypeCodeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialPrograms", objectName = "StateAtRiskCriterionTypeCode", objectId = StateAtRiskCriterionTypeCodeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StateAtRiskCriterionTypeCode
	#'
	#' This function creates a StateAtRiskCriterionTypeCode
	#' @param fieldNames The field values to give the created StateAtRiskCriterionTypeCode. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A newly created StateAtRiskCriterionTypeCode
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStateAtRiskCriterionTypeCode <- function(Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialPrograms", objectName = "StateAtRiskCriterionTypeCode", body = list(DataObject = body), searchFields = append("StateAtRiskCriterionTypeCodeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StateAtRiskCriterionTypeCode
	#'
	#' This function modifies a StateAtRiskCriterionTypeCode
	#' @param fieldNames The field values to give the modified StateAtRiskCriterionTypeCode. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The modified StateAtRiskCriterionTypeCode
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStateAtRiskCriterionTypeCode <- function(StateAtRiskCriterionTypeCodeID, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialPrograms", objectName = "StateAtRiskCriterionTypeCode", objectId = StateAtRiskCriterionTypeCodeID, body = list(DataObject = body), searchFields = append("StateAtRiskCriterionTypeCodeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CarlPerkinsMNS
	#'
	#' This function returns a dataframe or json object of CarlPerkinsMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CarlPerkinsMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CarlPerkinsMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CarlPerkinsMN') to get more field paths.
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
	#' @concept Special Programs
	#' @return A list of CarlPerkinsMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCarlPerkinsMNS <- function(searchConditionsList = NULL, CarlPerkinsMNID = F, DistrictID = F, SchoolYearID = F, IsTeenParent = F, IsDisplacedHomemaker = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialPrograms", objectName = "CarlPerkinsMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CarlPerkinsMN
	#'
	#' This function returns a dataframe or json object of a CarlPerkinsMN
	#' @param CarlPerkinsMNID The ID of the CarlPerkinsMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CarlPerkinsMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CarlPerkinsMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CarlPerkinsMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A dataframe or of CarlPerkinsMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCarlPerkinsMN <- function(CarlPerkinsMNID, DistrictID = F, SchoolYearID = F, IsTeenParent = F, IsDisplacedHomemaker = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CarlPerkinsMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialPrograms", objectName = "CarlPerkinsMN", objectId = CarlPerkinsMNID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CarlPerkinsMN
	#'
	#' This function deletes a CarlPerkinsMN
	#' @param CarlPerkinsMNID The ID of the CarlPerkinsMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The CarlPerkinsMNID of the deleted CarlPerkinsMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCarlPerkinsMN <- function(CarlPerkinsMNID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialPrograms", objectName = "CarlPerkinsMN", objectId = CarlPerkinsMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CarlPerkinsMN
	#'
	#' This function creates a CarlPerkinsMN
	#' @param fieldNames The field values to give the created CarlPerkinsMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A newly created CarlPerkinsMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCarlPerkinsMN <- function(DistrictID = NULL, SchoolYearID = NULL, IsTeenParent = NULL, IsDisplacedHomemaker = NULL, StudentID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialPrograms", objectName = "CarlPerkinsMN", body = list(DataObject = body), searchFields = append("CarlPerkinsMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CarlPerkinsMN
	#'
	#' This function modifies a CarlPerkinsMN
	#' @param fieldNames The field values to give the modified CarlPerkinsMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The modified CarlPerkinsMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCarlPerkinsMN <- function(CarlPerkinsMNID, DistrictID = NULL, SchoolYearID = NULL, IsTeenParent = NULL, IsDisplacedHomemaker = NULL, StudentID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialPrograms", objectName = "CarlPerkinsMN", objectId = CarlPerkinsMNID, body = list(DataObject = body), searchFields = append("CarlPerkinsMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Section504s
	#'
	#' This function returns a dataframe or json object of Section504s
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Section504s. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Section504s.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Section504') to get more field paths.
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
	#' @concept Special Programs
	#' @return A list of Section504s
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSection504s <- function(searchConditionsList = NULL, Section504MNID = F, EntryComment = F, ExitComment = F, Section504ID = F, DistrictID = F, StartDate = F, EndDate = F, IsActive = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AttachmentCount = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialPrograms", objectName = "Section504", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Section504
	#'
	#' This function returns a dataframe or json object of a Section504
	#' @param Section504ID The ID of the Section504 to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Section504. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Section504.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Section504') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A dataframe or of Section504
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSection504 <- function(Section504ID, Section504MNID = F, EntryComment = F, ExitComment = F, DistrictID = F, StartDate = F, EndDate = F, IsActive = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AttachmentCount = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "Section504ID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialPrograms", objectName = "Section504", objectId = Section504ID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Section504
	#'
	#' This function deletes a Section504
	#' @param Section504ID The ID of the Section504 to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The Section504ID of the deleted Section504.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSection504 <- function(Section504ID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialPrograms", objectName = "Section504", objectId = Section504ID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Section504
	#'
	#' This function creates a Section504
	#' @param fieldNames The field values to give the created Section504. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A newly created Section504
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSection504 <- function(EntryComment = NULL, ExitComment = NULL, DistrictID = NULL, StartDate = NULL, EndDate = NULL, StudentID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialPrograms", objectName = "Section504", body = list(DataObject = body), searchFields = append("Section504ID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Section504
	#'
	#' This function modifies a Section504
	#' @param fieldNames The field values to give the modified Section504. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The modified Section504
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySection504 <- function(Section504ID, EntryComment = NULL, ExitComment = NULL, DistrictID = NULL, StartDate = NULL, EndDate = NULL, StudentID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialPrograms", objectName = "Section504", objectId = Section504ID, body = list(DataObject = body), searchFields = append("Section504ID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List MigrantMNS
	#'
	#' This function returns a dataframe or json object of MigrantMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given MigrantMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MigrantMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MigrantMN') to get more field paths.
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
	#' @concept Special Programs
	#' @return A list of MigrantMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listMigrantMNS <- function(searchConditionsList = NULL, MigrantMNID = F, DistrictID = F, EntryComment = F, ExitComment = F, StartDate = F, EndDate = F, IsActive = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialPrograms", objectName = "MigrantMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a MigrantMN
	#'
	#' This function returns a dataframe or json object of a MigrantMN
	#' @param MigrantMNID The ID of the MigrantMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given MigrantMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MigrantMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MigrantMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A dataframe or of MigrantMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getMigrantMN <- function(MigrantMNID, DistrictID = F, EntryComment = F, ExitComment = F, StartDate = F, EndDate = F, IsActive = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "MigrantMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialPrograms", objectName = "MigrantMN", objectId = MigrantMNID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a MigrantMN
	#'
	#' This function deletes a MigrantMN
	#' @param MigrantMNID The ID of the MigrantMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The MigrantMNID of the deleted MigrantMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteMigrantMN <- function(MigrantMNID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialPrograms", objectName = "MigrantMN", objectId = MigrantMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a MigrantMN
	#'
	#' This function creates a MigrantMN
	#' @param fieldNames The field values to give the created MigrantMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A newly created MigrantMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createMigrantMN <- function(DistrictID = NULL, EntryComment = NULL, ExitComment = NULL, StartDate = NULL, EndDate = NULL, StudentID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialPrograms", objectName = "MigrantMN", body = list(DataObject = body), searchFields = append("MigrantMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a MigrantMN
	#'
	#' This function modifies a MigrantMN
	#' @param fieldNames The field values to give the modified MigrantMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The modified MigrantMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyMigrantMN <- function(MigrantMNID, DistrictID = NULL, EntryComment = NULL, ExitComment = NULL, StartDate = NULL, EndDate = NULL, StudentID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialPrograms", objectName = "MigrantMN", objectId = MigrantMNID, body = list(DataObject = body), searchFields = append("MigrantMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List GraduationRequirementMNS
	#'
	#' This function returns a dataframe or json object of GraduationRequirementMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given GraduationRequirementMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the GraduationRequirementMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('GraduationRequirementMN') to get more field paths.
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
	#' @concept Special Programs
	#' @return A list of GraduationRequirementMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listGraduationRequirementMNS <- function(searchConditionsList = NULL, GraduationRequirementMNID = F, EntryComment = F, ExitComment = F, StateGraduationRequirementMNID = F, StateGraduationRequirementMethodMNID = F, MetGraduationRequirement = F, StartDate = F, EndDate = F, IsActive = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DistrictID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialPrograms", objectName = "GraduationRequirementMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a GraduationRequirementMN
	#'
	#' This function returns a dataframe or json object of a GraduationRequirementMN
	#' @param GraduationRequirementMNID The ID of the GraduationRequirementMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given GraduationRequirementMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the GraduationRequirementMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('GraduationRequirementMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A dataframe or of GraduationRequirementMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getGraduationRequirementMN <- function(GraduationRequirementMNID, EntryComment = F, ExitComment = F, StateGraduationRequirementMNID = F, StateGraduationRequirementMethodMNID = F, MetGraduationRequirement = F, StartDate = F, EndDate = F, IsActive = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DistrictID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "GraduationRequirementMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialPrograms", objectName = "GraduationRequirementMN", objectId = GraduationRequirementMNID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a GraduationRequirementMN
	#'
	#' This function deletes a GraduationRequirementMN
	#' @param GraduationRequirementMNID The ID of the GraduationRequirementMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The GraduationRequirementMNID of the deleted GraduationRequirementMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteGraduationRequirementMN <- function(GraduationRequirementMNID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialPrograms", objectName = "GraduationRequirementMN", objectId = GraduationRequirementMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a GraduationRequirementMN
	#'
	#' This function creates a GraduationRequirementMN
	#' @param fieldNames The field values to give the created GraduationRequirementMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A newly created GraduationRequirementMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createGraduationRequirementMN <- function(EntryComment = NULL, ExitComment = NULL, StateGraduationRequirementMNID = NULL, StateGraduationRequirementMethodMNID = NULL, MetGraduationRequirement = NULL, StartDate = NULL, EndDate = NULL, StudentID = NULL, DistrictID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialPrograms", objectName = "GraduationRequirementMN", body = list(DataObject = body), searchFields = append("GraduationRequirementMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a GraduationRequirementMN
	#'
	#' This function modifies a GraduationRequirementMN
	#' @param fieldNames The field values to give the modified GraduationRequirementMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The modified GraduationRequirementMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyGraduationRequirementMN <- function(GraduationRequirementMNID, EntryComment = NULL, ExitComment = NULL, StateGraduationRequirementMNID = NULL, StateGraduationRequirementMethodMNID = NULL, MetGraduationRequirement = NULL, StartDate = NULL, EndDate = NULL, StudentID = NULL, DistrictID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialPrograms", objectName = "GraduationRequirementMN", objectId = GraduationRequirementMNID, body = list(DataObject = body), searchFields = append("GraduationRequirementMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempMassDeleteSpecialPrograms
	#'
	#' This function returns a dataframe or json object of TempMassDeleteSpecialPrograms
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempMassDeleteSpecialPrograms. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempMassDeleteSpecialPrograms.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempMassDeleteSpecialProgram') to get more field paths.
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
	#' @concept Special Programs
	#' @return A list of TempMassDeleteSpecialPrograms
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempMassDeleteSpecialPrograms <- function(searchConditionsList = NULL, TempMassDeleteSpecialProgramID = F, RecordType = F, RecordTypeFriendlyName = F, StudentID = F, StudentFullName = F, StartDate = F, EndDate = F, AffectedPrimaryKey = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialPrograms", objectName = "TempMassDeleteSpecialProgram", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempMassDeleteSpecialProgram
	#'
	#' This function returns a dataframe or json object of a TempMassDeleteSpecialProgram
	#' @param TempMassDeleteSpecialProgramID The ID of the TempMassDeleteSpecialProgram to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempMassDeleteSpecialProgram. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempMassDeleteSpecialProgram.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempMassDeleteSpecialProgram') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A dataframe or of TempMassDeleteSpecialProgram
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempMassDeleteSpecialProgram <- function(TempMassDeleteSpecialProgramID, RecordType = F, RecordTypeFriendlyName = F, StudentID = F, StudentFullName = F, StartDate = F, EndDate = F, AffectedPrimaryKey = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempMassDeleteSpecialProgramID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialPrograms", objectName = "TempMassDeleteSpecialProgram", objectId = TempMassDeleteSpecialProgramID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempMassDeleteSpecialProgram
	#'
	#' This function deletes a TempMassDeleteSpecialProgram
	#' @param TempMassDeleteSpecialProgramID The ID of the TempMassDeleteSpecialProgram to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The TempMassDeleteSpecialProgramID of the deleted TempMassDeleteSpecialProgram.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempMassDeleteSpecialProgram <- function(TempMassDeleteSpecialProgramID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialPrograms", objectName = "TempMassDeleteSpecialProgram", objectId = TempMassDeleteSpecialProgramID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempMassDeleteSpecialProgram
	#'
	#' This function creates a TempMassDeleteSpecialProgram
	#' @param fieldNames The field values to give the created TempMassDeleteSpecialProgram. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A newly created TempMassDeleteSpecialProgram
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempMassDeleteSpecialProgram <- function(RecordType = NULL, RecordTypeFriendlyName = NULL, StudentID = NULL, StudentFullName = NULL, StartDate = NULL, EndDate = NULL, AffectedPrimaryKey = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialPrograms", objectName = "TempMassDeleteSpecialProgram", body = list(DataObject = body), searchFields = append("TempMassDeleteSpecialProgramID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempMassDeleteSpecialProgram
	#'
	#' This function modifies a TempMassDeleteSpecialProgram
	#' @param fieldNames The field values to give the modified TempMassDeleteSpecialProgram. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The modified TempMassDeleteSpecialProgram
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempMassDeleteSpecialProgram <- function(TempMassDeleteSpecialProgramID, RecordType = NULL, RecordTypeFriendlyName = NULL, StudentID = NULL, StudentFullName = NULL, StartDate = NULL, EndDate = NULL, AffectedPrimaryKey = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialPrograms", objectName = "TempMassDeleteSpecialProgram", objectId = TempMassDeleteSpecialProgramID, body = list(DataObject = body), searchFields = append("TempMassDeleteSpecialProgramID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List MassAddSpecialProgramsRunHistories
	#'
	#' This function returns a dataframe or json object of MassAddSpecialProgramsRunHistories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given MassAddSpecialProgramsRunHistories. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MassAddSpecialProgramsRunHistories.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MassAddSpecialProgramsRunHistory') to get more field paths.
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
	#' @concept Special Programs
	#' @return A list of MassAddSpecialProgramsRunHistories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listMassAddSpecialProgramsRunHistories <- function(searchConditionsList = NULL, MassAddSpecialProgramsRunHistoryID = F, SchoolYearID = F, DistrictID = F, StartTime = F, EndTime = F, FilterParameters = F, RunReason = F, ValueParameters = F, IsLocked = F, Status = F, RunData = F, UserIDCanceledBy = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, RunParameters = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialPrograms", objectName = "MassAddSpecialProgramsRunHistory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a MassAddSpecialProgramsRunHistory
	#'
	#' This function returns a dataframe or json object of a MassAddSpecialProgramsRunHistory
	#' @param MassAddSpecialProgramsRunHistoryID The ID of the MassAddSpecialProgramsRunHistory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given MassAddSpecialProgramsRunHistory. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MassAddSpecialProgramsRunHistory.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MassAddSpecialProgramsRunHistory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A dataframe or of MassAddSpecialProgramsRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getMassAddSpecialProgramsRunHistory <- function(MassAddSpecialProgramsRunHistoryID, SchoolYearID = F, DistrictID = F, StartTime = F, EndTime = F, FilterParameters = F, RunReason = F, ValueParameters = F, IsLocked = F, Status = F, RunData = F, UserIDCanceledBy = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, RunParameters = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "MassAddSpecialProgramsRunHistoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialPrograms", objectName = "MassAddSpecialProgramsRunHistory", objectId = MassAddSpecialProgramsRunHistoryID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a MassAddSpecialProgramsRunHistory
	#'
	#' This function deletes a MassAddSpecialProgramsRunHistory
	#' @param MassAddSpecialProgramsRunHistoryID The ID of the MassAddSpecialProgramsRunHistory to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The MassAddSpecialProgramsRunHistoryID of the deleted MassAddSpecialProgramsRunHistory.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteMassAddSpecialProgramsRunHistory <- function(MassAddSpecialProgramsRunHistoryID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialPrograms", objectName = "MassAddSpecialProgramsRunHistory", objectId = MassAddSpecialProgramsRunHistoryID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a MassAddSpecialProgramsRunHistory
	#'
	#' This function creates a MassAddSpecialProgramsRunHistory
	#' @param fieldNames The field values to give the created MassAddSpecialProgramsRunHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A newly created MassAddSpecialProgramsRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createMassAddSpecialProgramsRunHistory <- function(SchoolYearID = NULL, DistrictID = NULL, StartTime = NULL, EndTime = NULL, FilterParameters = NULL, RunReason = NULL, ValueParameters = NULL, Status = NULL, UserIDCanceledBy = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialPrograms", objectName = "MassAddSpecialProgramsRunHistory", body = list(DataObject = body), searchFields = append("MassAddSpecialProgramsRunHistoryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a MassAddSpecialProgramsRunHistory
	#'
	#' This function modifies a MassAddSpecialProgramsRunHistory
	#' @param fieldNames The field values to give the modified MassAddSpecialProgramsRunHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The modified MassAddSpecialProgramsRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyMassAddSpecialProgramsRunHistory <- function(MassAddSpecialProgramsRunHistoryID, SchoolYearID = NULL, DistrictID = NULL, StartTime = NULL, EndTime = NULL, FilterParameters = NULL, RunReason = NULL, ValueParameters = NULL, Status = NULL, UserIDCanceledBy = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialPrograms", objectName = "MassAddSpecialProgramsRunHistory", objectId = MassAddSpecialProgramsRunHistoryID, body = list(DataObject = body), searchFields = append("MassAddSpecialProgramsRunHistoryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List MassDeleteSpecialProgramsRunHistories
	#'
	#' This function returns a dataframe or json object of MassDeleteSpecialProgramsRunHistories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given MassDeleteSpecialProgramsRunHistories. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MassDeleteSpecialProgramsRunHistories.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MassDeleteSpecialProgramsRunHistory') to get more field paths.
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
	#' @concept Special Programs
	#' @return A list of MassDeleteSpecialProgramsRunHistories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listMassDeleteSpecialProgramsRunHistories <- function(searchConditionsList = NULL, MassDeleteSpecialProgramsRunHistoryID = F, SchoolYearID = F, DistrictID = F, StartTime = F, EndTime = F, FilterParameters = F, RunReason = F, IsLocked = F, Status = F, RunData = F, UserIDCanceledBy = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ValueParameters = F, RunParameters = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialPrograms", objectName = "MassDeleteSpecialProgramsRunHistory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a MassDeleteSpecialProgramsRunHistory
	#'
	#' This function returns a dataframe or json object of a MassDeleteSpecialProgramsRunHistory
	#' @param MassDeleteSpecialProgramsRunHistoryID The ID of the MassDeleteSpecialProgramsRunHistory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given MassDeleteSpecialProgramsRunHistory. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MassDeleteSpecialProgramsRunHistory.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MassDeleteSpecialProgramsRunHistory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A dataframe or of MassDeleteSpecialProgramsRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getMassDeleteSpecialProgramsRunHistory <- function(MassDeleteSpecialProgramsRunHistoryID, SchoolYearID = F, DistrictID = F, StartTime = F, EndTime = F, FilterParameters = F, RunReason = F, IsLocked = F, Status = F, RunData = F, UserIDCanceledBy = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ValueParameters = F, RunParameters = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "MassDeleteSpecialProgramsRunHistoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialPrograms", objectName = "MassDeleteSpecialProgramsRunHistory", objectId = MassDeleteSpecialProgramsRunHistoryID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a MassDeleteSpecialProgramsRunHistory
	#'
	#' This function deletes a MassDeleteSpecialProgramsRunHistory
	#' @param MassDeleteSpecialProgramsRunHistoryID The ID of the MassDeleteSpecialProgramsRunHistory to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The MassDeleteSpecialProgramsRunHistoryID of the deleted MassDeleteSpecialProgramsRunHistory.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteMassDeleteSpecialProgramsRunHistory <- function(MassDeleteSpecialProgramsRunHistoryID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialPrograms", objectName = "MassDeleteSpecialProgramsRunHistory", objectId = MassDeleteSpecialProgramsRunHistoryID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a MassDeleteSpecialProgramsRunHistory
	#'
	#' This function creates a MassDeleteSpecialProgramsRunHistory
	#' @param fieldNames The field values to give the created MassDeleteSpecialProgramsRunHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A newly created MassDeleteSpecialProgramsRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createMassDeleteSpecialProgramsRunHistory <- function(SchoolYearID = NULL, DistrictID = NULL, StartTime = NULL, EndTime = NULL, FilterParameters = NULL, RunReason = NULL, Status = NULL, UserIDCanceledBy = NULL, ValueParameters = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialPrograms", objectName = "MassDeleteSpecialProgramsRunHistory", body = list(DataObject = body), searchFields = append("MassDeleteSpecialProgramsRunHistoryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a MassDeleteSpecialProgramsRunHistory
	#'
	#' This function modifies a MassDeleteSpecialProgramsRunHistory
	#' @param fieldNames The field values to give the modified MassDeleteSpecialProgramsRunHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The modified MassDeleteSpecialProgramsRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyMassDeleteSpecialProgramsRunHistory <- function(MassDeleteSpecialProgramsRunHistoryID, SchoolYearID = NULL, DistrictID = NULL, StartTime = NULL, EndTime = NULL, FilterParameters = NULL, RunReason = NULL, Status = NULL, UserIDCanceledBy = NULL, ValueParameters = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialPrograms", objectName = "MassDeleteSpecialProgramsRunHistory", objectId = MassDeleteSpecialProgramsRunHistoryID, body = list(DataObject = body), searchFields = append("MassDeleteSpecialProgramsRunHistoryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List MassEndSpecialProgramsRunHistories
	#'
	#' This function returns a dataframe or json object of MassEndSpecialProgramsRunHistories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given MassEndSpecialProgramsRunHistories. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MassEndSpecialProgramsRunHistories.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MassEndSpecialProgramsRunHistory') to get more field paths.
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
	#' @concept Special Programs
	#' @return A list of MassEndSpecialProgramsRunHistories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listMassEndSpecialProgramsRunHistories <- function(searchConditionsList = NULL, MassEndSpecialProgramsRunHistoryID = F, SchoolYearID = F, DistrictID = F, StartTime = F, EndTime = F, FilterParameters = F, RunReason = F, ValueParameters = F, IsLocked = F, Status = F, RunData = F, UserIDCanceledBy = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, RunParameters = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialPrograms", objectName = "MassEndSpecialProgramsRunHistory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a MassEndSpecialProgramsRunHistory
	#'
	#' This function returns a dataframe or json object of a MassEndSpecialProgramsRunHistory
	#' @param MassEndSpecialProgramsRunHistoryID The ID of the MassEndSpecialProgramsRunHistory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given MassEndSpecialProgramsRunHistory. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MassEndSpecialProgramsRunHistory.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MassEndSpecialProgramsRunHistory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A dataframe or of MassEndSpecialProgramsRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getMassEndSpecialProgramsRunHistory <- function(MassEndSpecialProgramsRunHistoryID, SchoolYearID = F, DistrictID = F, StartTime = F, EndTime = F, FilterParameters = F, RunReason = F, ValueParameters = F, IsLocked = F, Status = F, RunData = F, UserIDCanceledBy = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, RunParameters = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "MassEndSpecialProgramsRunHistoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialPrograms", objectName = "MassEndSpecialProgramsRunHistory", objectId = MassEndSpecialProgramsRunHistoryID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a MassEndSpecialProgramsRunHistory
	#'
	#' This function deletes a MassEndSpecialProgramsRunHistory
	#' @param MassEndSpecialProgramsRunHistoryID The ID of the MassEndSpecialProgramsRunHistory to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The MassEndSpecialProgramsRunHistoryID of the deleted MassEndSpecialProgramsRunHistory.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteMassEndSpecialProgramsRunHistory <- function(MassEndSpecialProgramsRunHistoryID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialPrograms", objectName = "MassEndSpecialProgramsRunHistory", objectId = MassEndSpecialProgramsRunHistoryID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a MassEndSpecialProgramsRunHistory
	#'
	#' This function creates a MassEndSpecialProgramsRunHistory
	#' @param fieldNames The field values to give the created MassEndSpecialProgramsRunHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A newly created MassEndSpecialProgramsRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createMassEndSpecialProgramsRunHistory <- function(SchoolYearID = NULL, DistrictID = NULL, StartTime = NULL, EndTime = NULL, FilterParameters = NULL, RunReason = NULL, ValueParameters = NULL, Status = NULL, UserIDCanceledBy = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialPrograms", objectName = "MassEndSpecialProgramsRunHistory", body = list(DataObject = body), searchFields = append("MassEndSpecialProgramsRunHistoryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a MassEndSpecialProgramsRunHistory
	#'
	#' This function modifies a MassEndSpecialProgramsRunHistory
	#' @param fieldNames The field values to give the modified MassEndSpecialProgramsRunHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The modified MassEndSpecialProgramsRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyMassEndSpecialProgramsRunHistory <- function(MassEndSpecialProgramsRunHistoryID, SchoolYearID = NULL, DistrictID = NULL, StartTime = NULL, EndTime = NULL, FilterParameters = NULL, RunReason = NULL, ValueParameters = NULL, Status = NULL, UserIDCanceledBy = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialPrograms", objectName = "MassEndSpecialProgramsRunHistory", objectId = MassEndSpecialProgramsRunHistoryID, body = list(DataObject = body), searchFields = append("MassEndSpecialProgramsRunHistoryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempMassAddSpecialPrograms
	#'
	#' This function returns a dataframe or json object of TempMassAddSpecialPrograms
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempMassAddSpecialPrograms. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempMassAddSpecialPrograms.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempMassAddSpecialProgram') to get more field paths.
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
	#' @concept Special Programs
	#' @return A list of TempMassAddSpecialPrograms
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempMassAddSpecialPrograms <- function(searchConditionsList = NULL, TempMassAddSpecialProgramID = F, SpecialProgramDBTableName = F, RecordType = F, RecordTypeFriendlyName = F, StudentID = F, StudentFullName = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StudentDefaultEntityID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialPrograms", objectName = "TempMassAddSpecialProgram", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempMassAddSpecialProgram
	#'
	#' This function returns a dataframe or json object of a TempMassAddSpecialProgram
	#' @param TempMassAddSpecialProgramID The ID of the TempMassAddSpecialProgram to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempMassAddSpecialProgram. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempMassAddSpecialProgram.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempMassAddSpecialProgram') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A dataframe or of TempMassAddSpecialProgram
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempMassAddSpecialProgram <- function(TempMassAddSpecialProgramID, SpecialProgramDBTableName = F, RecordType = F, RecordTypeFriendlyName = F, StudentID = F, StudentFullName = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StudentDefaultEntityID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempMassAddSpecialProgramID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialPrograms", objectName = "TempMassAddSpecialProgram", objectId = TempMassAddSpecialProgramID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempMassAddSpecialProgram
	#'
	#' This function deletes a TempMassAddSpecialProgram
	#' @param TempMassAddSpecialProgramID The ID of the TempMassAddSpecialProgram to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The TempMassAddSpecialProgramID of the deleted TempMassAddSpecialProgram.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempMassAddSpecialProgram <- function(TempMassAddSpecialProgramID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialPrograms", objectName = "TempMassAddSpecialProgram", objectId = TempMassAddSpecialProgramID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempMassAddSpecialProgram
	#'
	#' This function creates a TempMassAddSpecialProgram
	#' @param fieldNames The field values to give the created TempMassAddSpecialProgram. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A newly created TempMassAddSpecialProgram
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempMassAddSpecialProgram <- function(SpecialProgramDBTableName = NULL, RecordType = NULL, RecordTypeFriendlyName = NULL, StudentID = NULL, StudentFullName = NULL, StudentDefaultEntityID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialPrograms", objectName = "TempMassAddSpecialProgram", body = list(DataObject = body), searchFields = append("TempMassAddSpecialProgramID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempMassAddSpecialProgram
	#'
	#' This function modifies a TempMassAddSpecialProgram
	#' @param fieldNames The field values to give the modified TempMassAddSpecialProgram. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The modified TempMassAddSpecialProgram
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempMassAddSpecialProgram <- function(TempMassAddSpecialProgramID, SpecialProgramDBTableName = NULL, RecordType = NULL, RecordTypeFriendlyName = NULL, StudentID = NULL, StudentFullName = NULL, StudentDefaultEntityID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialPrograms", objectName = "TempMassAddSpecialProgram", objectId = TempMassAddSpecialProgramID, body = list(DataObject = body), searchFields = append("TempMassAddSpecialProgramID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempMassAddSpecialProgramFields
	#'
	#' This function returns a dataframe or json object of TempMassAddSpecialProgramFields
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempMassAddSpecialProgramFields. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempMassAddSpecialProgramFields.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempMassAddSpecialProgramField') to get more field paths.
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
	#' @concept Special Programs
	#' @return A list of TempMassAddSpecialProgramFields
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempMassAddSpecialProgramFields <- function(searchConditionsList = NULL, TempMassAddSpecialProgramFieldID = F, TempMassAddSpecialProgramID = F, FieldName = F, FriendlyFieldName = F, FieldValue = F, FriendlyFieldValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsSpecialProgramChildField = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialPrograms", objectName = "TempMassAddSpecialProgramField", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempMassAddSpecialProgramField
	#'
	#' This function returns a dataframe or json object of a TempMassAddSpecialProgramField
	#' @param TempMassAddSpecialProgramFieldID The ID of the TempMassAddSpecialProgramField to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempMassAddSpecialProgramField. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempMassAddSpecialProgramField.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempMassAddSpecialProgramField') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A dataframe or of TempMassAddSpecialProgramField
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempMassAddSpecialProgramField <- function(TempMassAddSpecialProgramFieldID, TempMassAddSpecialProgramID = F, FieldName = F, FriendlyFieldName = F, FieldValue = F, FriendlyFieldValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsSpecialProgramChildField = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempMassAddSpecialProgramFieldID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialPrograms", objectName = "TempMassAddSpecialProgramField", objectId = TempMassAddSpecialProgramFieldID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempMassAddSpecialProgramField
	#'
	#' This function deletes a TempMassAddSpecialProgramField
	#' @param TempMassAddSpecialProgramFieldID The ID of the TempMassAddSpecialProgramField to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The TempMassAddSpecialProgramFieldID of the deleted TempMassAddSpecialProgramField.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempMassAddSpecialProgramField <- function(TempMassAddSpecialProgramFieldID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialPrograms", objectName = "TempMassAddSpecialProgramField", objectId = TempMassAddSpecialProgramFieldID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempMassAddSpecialProgramField
	#'
	#' This function creates a TempMassAddSpecialProgramField
	#' @param fieldNames The field values to give the created TempMassAddSpecialProgramField. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A newly created TempMassAddSpecialProgramField
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempMassAddSpecialProgramField <- function(TempMassAddSpecialProgramID = NULL, FieldName = NULL, FriendlyFieldName = NULL, FieldValue = NULL, FriendlyFieldValue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialPrograms", objectName = "TempMassAddSpecialProgramField", body = list(DataObject = body), searchFields = append("TempMassAddSpecialProgramFieldID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempMassAddSpecialProgramField
	#'
	#' This function modifies a TempMassAddSpecialProgramField
	#' @param fieldNames The field values to give the modified TempMassAddSpecialProgramField. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The modified TempMassAddSpecialProgramField
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempMassAddSpecialProgramField <- function(TempMassAddSpecialProgramFieldID, TempMassAddSpecialProgramID = NULL, FieldName = NULL, FriendlyFieldName = NULL, FieldValue = NULL, FriendlyFieldValue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialPrograms", objectName = "TempMassAddSpecialProgramField", objectId = TempMassAddSpecialProgramFieldID, body = list(DataObject = body), searchFields = append("TempMassAddSpecialProgramFieldID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempMassEndSpecialPrograms
	#'
	#' This function returns a dataframe or json object of TempMassEndSpecialPrograms
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempMassEndSpecialPrograms. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempMassEndSpecialPrograms.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempMassEndSpecialProgram') to get more field paths.
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
	#' @concept Special Programs
	#' @return A list of TempMassEndSpecialPrograms
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempMassEndSpecialPrograms <- function(searchConditionsList = NULL, TempMassEndSpecialProgramID = F, RecordType = F, RecordTypeFriendlyName = F, SpecialProgramID = F, StudentID = F, StudentFullName = F, StartDate = F, EndDate = F, AffectedPrimaryKey = F, EndDateFieldName = F, StateSpecialProgramExitReasonWAID = F, StateSpecialProgramExitReasonCode = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StateAlternativeEducationOutcomeINID = F, StateAlternativeEducationOutcomeINCode = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialPrograms", objectName = "TempMassEndSpecialProgram", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempMassEndSpecialProgram
	#'
	#' This function returns a dataframe or json object of a TempMassEndSpecialProgram
	#' @param TempMassEndSpecialProgramID The ID of the TempMassEndSpecialProgram to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempMassEndSpecialProgram. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempMassEndSpecialProgram.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempMassEndSpecialProgram') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A dataframe or of TempMassEndSpecialProgram
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempMassEndSpecialProgram <- function(TempMassEndSpecialProgramID, RecordType = F, RecordTypeFriendlyName = F, SpecialProgramID = F, StudentID = F, StudentFullName = F, StartDate = F, EndDate = F, AffectedPrimaryKey = F, EndDateFieldName = F, StateSpecialProgramExitReasonWAID = F, StateSpecialProgramExitReasonCode = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StateAlternativeEducationOutcomeINID = F, StateAlternativeEducationOutcomeINCode = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempMassEndSpecialProgramID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialPrograms", objectName = "TempMassEndSpecialProgram", objectId = TempMassEndSpecialProgramID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempMassEndSpecialProgram
	#'
	#' This function deletes a TempMassEndSpecialProgram
	#' @param TempMassEndSpecialProgramID The ID of the TempMassEndSpecialProgram to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The TempMassEndSpecialProgramID of the deleted TempMassEndSpecialProgram.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempMassEndSpecialProgram <- function(TempMassEndSpecialProgramID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialPrograms", objectName = "TempMassEndSpecialProgram", objectId = TempMassEndSpecialProgramID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempMassEndSpecialProgram
	#'
	#' This function creates a TempMassEndSpecialProgram
	#' @param fieldNames The field values to give the created TempMassEndSpecialProgram. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A newly created TempMassEndSpecialProgram
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempMassEndSpecialProgram <- function(RecordType = NULL, RecordTypeFriendlyName = NULL, SpecialProgramID = NULL, StudentID = NULL, StudentFullName = NULL, StartDate = NULL, EndDate = NULL, AffectedPrimaryKey = NULL, EndDateFieldName = NULL, StateSpecialProgramExitReasonWAID = NULL, StateSpecialProgramExitReasonCode = NULL, StateAlternativeEducationOutcomeINID = NULL, StateAlternativeEducationOutcomeINCode = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialPrograms", objectName = "TempMassEndSpecialProgram", body = list(DataObject = body), searchFields = append("TempMassEndSpecialProgramID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempMassEndSpecialProgram
	#'
	#' This function modifies a TempMassEndSpecialProgram
	#' @param fieldNames The field values to give the modified TempMassEndSpecialProgram. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The modified TempMassEndSpecialProgram
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempMassEndSpecialProgram <- function(TempMassEndSpecialProgramID, RecordType = NULL, RecordTypeFriendlyName = NULL, SpecialProgramID = NULL, StudentID = NULL, StudentFullName = NULL, StartDate = NULL, EndDate = NULL, AffectedPrimaryKey = NULL, EndDateFieldName = NULL, StateSpecialProgramExitReasonWAID = NULL, StateSpecialProgramExitReasonCode = NULL, StateAlternativeEducationOutcomeINID = NULL, StateAlternativeEducationOutcomeINCode = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialPrograms", objectName = "TempMassEndSpecialProgram", objectId = TempMassEndSpecialProgramID, body = list(DataObject = body), searchFields = append("TempMassEndSpecialProgramID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempMassSpecialProgramUtilityErrors
	#'
	#' This function returns a dataframe or json object of TempMassSpecialProgramUtilityErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempMassSpecialProgramUtilityErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempMassSpecialProgramUtilityErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempMassSpecialProgramUtilityError') to get more field paths.
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
	#' @concept Special Programs
	#' @return A list of TempMassSpecialProgramUtilityErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempMassSpecialProgramUtilityErrors <- function(searchConditionsList = NULL, TempMassSpecialProgramUtilityErrorID = F, SourceDescription = F, RecordTypeFriendlyName = F, StudentID = F, StudentFullName = F, FailureReason = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialPrograms", objectName = "TempMassSpecialProgramUtilityError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempMassSpecialProgramUtilityError
	#'
	#' This function returns a dataframe or json object of a TempMassSpecialProgramUtilityError
	#' @param TempMassSpecialProgramUtilityErrorID The ID of the TempMassSpecialProgramUtilityError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempMassSpecialProgramUtilityError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempMassSpecialProgramUtilityError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempMassSpecialProgramUtilityError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A dataframe or of TempMassSpecialProgramUtilityError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempMassSpecialProgramUtilityError <- function(TempMassSpecialProgramUtilityErrorID, SourceDescription = F, RecordTypeFriendlyName = F, StudentID = F, StudentFullName = F, FailureReason = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempMassSpecialProgramUtilityErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialPrograms", objectName = "TempMassSpecialProgramUtilityError", objectId = TempMassSpecialProgramUtilityErrorID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempMassSpecialProgramUtilityError
	#'
	#' This function deletes a TempMassSpecialProgramUtilityError
	#' @param TempMassSpecialProgramUtilityErrorID The ID of the TempMassSpecialProgramUtilityError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The TempMassSpecialProgramUtilityErrorID of the deleted TempMassSpecialProgramUtilityError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempMassSpecialProgramUtilityError <- function(TempMassSpecialProgramUtilityErrorID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialPrograms", objectName = "TempMassSpecialProgramUtilityError", objectId = TempMassSpecialProgramUtilityErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempMassSpecialProgramUtilityError
	#'
	#' This function creates a TempMassSpecialProgramUtilityError
	#' @param fieldNames The field values to give the created TempMassSpecialProgramUtilityError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A newly created TempMassSpecialProgramUtilityError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempMassSpecialProgramUtilityError <- function(SourceDescription = NULL, RecordTypeFriendlyName = NULL, StudentID = NULL, StudentFullName = NULL, FailureReason = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialPrograms", objectName = "TempMassSpecialProgramUtilityError", body = list(DataObject = body), searchFields = append("TempMassSpecialProgramUtilityErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempMassSpecialProgramUtilityError
	#'
	#' This function modifies a TempMassSpecialProgramUtilityError
	#' @param fieldNames The field values to give the modified TempMassSpecialProgramUtilityError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The modified TempMassSpecialProgramUtilityError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempMassSpecialProgramUtilityError <- function(TempMassSpecialProgramUtilityErrorID, SourceDescription = NULL, RecordTypeFriendlyName = NULL, StudentID = NULL, StudentFullName = NULL, FailureReason = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialPrograms", objectName = "TempMassSpecialProgramUtilityError", objectId = TempMassSpecialProgramUtilityErrorID, body = list(DataObject = body), searchFields = append("TempMassSpecialProgramUtilityErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempMassSpecialProgramUtilityRecordTypes
	#'
	#' This function returns a dataframe or json object of TempMassSpecialProgramUtilityRecordTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempMassSpecialProgramUtilityRecordTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempMassSpecialProgramUtilityRecordTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempMassSpecialProgramUtilityRecordType') to get more field paths.
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
	#' @concept Special Programs
	#' @return A list of TempMassSpecialProgramUtilityRecordTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempMassSpecialProgramUtilityRecordTypes <- function(searchConditionsList = NULL, TempMassSpecialProgramUtilityRecordTypeID = F, RecordTypeFriendlyName = F, SpecialProgramDBTableName = F, RecordType = F, SpecialProgramID = F, StartDateFieldName = F, EndDateFieldName = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialPrograms", objectName = "TempMassSpecialProgramUtilityRecordType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempMassSpecialProgramUtilityRecordType
	#'
	#' This function returns a dataframe or json object of a TempMassSpecialProgramUtilityRecordType
	#' @param TempMassSpecialProgramUtilityRecordTypeID The ID of the TempMassSpecialProgramUtilityRecordType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempMassSpecialProgramUtilityRecordType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempMassSpecialProgramUtilityRecordType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempMassSpecialProgramUtilityRecordType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A dataframe or of TempMassSpecialProgramUtilityRecordType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempMassSpecialProgramUtilityRecordType <- function(TempMassSpecialProgramUtilityRecordTypeID, RecordTypeFriendlyName = F, SpecialProgramDBTableName = F, RecordType = F, SpecialProgramID = F, StartDateFieldName = F, EndDateFieldName = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempMassSpecialProgramUtilityRecordTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialPrograms", objectName = "TempMassSpecialProgramUtilityRecordType", objectId = TempMassSpecialProgramUtilityRecordTypeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempMassSpecialProgramUtilityRecordType
	#'
	#' This function deletes a TempMassSpecialProgramUtilityRecordType
	#' @param TempMassSpecialProgramUtilityRecordTypeID The ID of the TempMassSpecialProgramUtilityRecordType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The TempMassSpecialProgramUtilityRecordTypeID of the deleted TempMassSpecialProgramUtilityRecordType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempMassSpecialProgramUtilityRecordType <- function(TempMassSpecialProgramUtilityRecordTypeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialPrograms", objectName = "TempMassSpecialProgramUtilityRecordType", objectId = TempMassSpecialProgramUtilityRecordTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempMassSpecialProgramUtilityRecordType
	#'
	#' This function creates a TempMassSpecialProgramUtilityRecordType
	#' @param fieldNames The field values to give the created TempMassSpecialProgramUtilityRecordType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A newly created TempMassSpecialProgramUtilityRecordType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempMassSpecialProgramUtilityRecordType <- function(RecordTypeFriendlyName = NULL, SpecialProgramDBTableName = NULL, RecordType = NULL, SpecialProgramID = NULL, StartDateFieldName = NULL, EndDateFieldName = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialPrograms", objectName = "TempMassSpecialProgramUtilityRecordType", body = list(DataObject = body), searchFields = append("TempMassSpecialProgramUtilityRecordTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempMassSpecialProgramUtilityRecordType
	#'
	#' This function modifies a TempMassSpecialProgramUtilityRecordType
	#' @param fieldNames The field values to give the modified TempMassSpecialProgramUtilityRecordType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The modified TempMassSpecialProgramUtilityRecordType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempMassSpecialProgramUtilityRecordType <- function(TempMassSpecialProgramUtilityRecordTypeID, RecordTypeFriendlyName = NULL, SpecialProgramDBTableName = NULL, RecordType = NULL, SpecialProgramID = NULL, StartDateFieldName = NULL, EndDateFieldName = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialPrograms", objectName = "TempMassSpecialProgramUtilityRecordType", objectId = TempMassSpecialProgramUtilityRecordTypeID, body = list(DataObject = body), searchFields = append("TempMassSpecialProgramUtilityRecordTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ESTARAsyleeRefugeeTXES
	#'
	#' This function returns a dataframe or json object of ESTARAsyleeRefugeeTXES
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ESTARAsyleeRefugeeTXES. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ESTARAsyleeRefugeeTXES.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ESTARAsyleeRefugeeTX') to get more field paths.
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
	#' @concept Special Programs
	#' @return A list of ESTARAsyleeRefugeeTXES
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listESTARAsyleeRefugeeTXES <- function(searchConditionsList = NULL, ESTARAsyleeRefugeeTXID = F, StateUnschooledAsyleeRefugeeCodeTXID = F, ESTARRunHistoryTXID = F, StudentID = F, FullNameLFM = F, LocalID = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialPrograms", objectName = "ESTARAsyleeRefugeeTX", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ESTARAsyleeRefugeeTX
	#'
	#' This function returns a dataframe or json object of an ESTARAsyleeRefugeeTX
	#' @param ESTARAsyleeRefugeeTXID The ID of the ESTARAsyleeRefugeeTX to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ESTARAsyleeRefugeeTX. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ESTARAsyleeRefugeeTX.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ESTARAsyleeRefugeeTX') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A dataframe or of ESTARAsyleeRefugeeTX
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getESTARAsyleeRefugeeTX <- function(ESTARAsyleeRefugeeTXID, StateUnschooledAsyleeRefugeeCodeTXID = F, ESTARRunHistoryTXID = F, StudentID = F, FullNameLFM = F, LocalID = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ESTARAsyleeRefugeeTXID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialPrograms", objectName = "ESTARAsyleeRefugeeTX", objectId = ESTARAsyleeRefugeeTXID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ESTARAsyleeRefugeeTX
	#'
	#' This function deletes an ESTARAsyleeRefugeeTX
	#' @param ESTARAsyleeRefugeeTXID The ID of the ESTARAsyleeRefugeeTX to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The ESTARAsyleeRefugeeTXID of the deleted ESTARAsyleeRefugeeTX.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteESTARAsyleeRefugeeTX <- function(ESTARAsyleeRefugeeTXID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialPrograms", objectName = "ESTARAsyleeRefugeeTX", objectId = ESTARAsyleeRefugeeTXID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ESTARAsyleeRefugeeTX
	#'
	#' This function creates an ESTARAsyleeRefugeeTX
	#' @param fieldNames The field values to give the created ESTARAsyleeRefugeeTX. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A newly created ESTARAsyleeRefugeeTX
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createESTARAsyleeRefugeeTX <- function(StateUnschooledAsyleeRefugeeCodeTXID = NULL, ESTARRunHistoryTXID = NULL, StudentID = NULL, FullNameLFM = NULL, LocalID = NULL, ProcessType = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialPrograms", objectName = "ESTARAsyleeRefugeeTX", body = list(DataObject = body), searchFields = append("ESTARAsyleeRefugeeTXID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ESTARAsyleeRefugeeTX
	#'
	#' This function modifies an ESTARAsyleeRefugeeTX
	#' @param fieldNames The field values to give the modified ESTARAsyleeRefugeeTX. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The modified ESTARAsyleeRefugeeTX
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyESTARAsyleeRefugeeTX <- function(ESTARAsyleeRefugeeTXID, StateUnschooledAsyleeRefugeeCodeTXID = NULL, ESTARRunHistoryTXID = NULL, StudentID = NULL, FullNameLFM = NULL, LocalID = NULL, ProcessType = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialPrograms", objectName = "ESTARAsyleeRefugeeTX", objectId = ESTARAsyleeRefugeeTXID, body = list(DataObject = body), searchFields = append("ESTARAsyleeRefugeeTXID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ECFEParentGuardianMNS
	#'
	#' This function returns a dataframe or json object of ECFEParentGuardianMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ECFEParentGuardianMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ECFEParentGuardianMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ECFEParentGuardianMN') to get more field paths.
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
	#' @concept Special Programs
	#' @return A list of ECFEParentGuardianMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listECFEParentGuardianMNS <- function(searchConditionsList = NULL, ECFEParentGuardianMNID = F, ECFEProgramRegistrationMNID = F, NameEntryType = F, NameID = F, FreeFormFirstName = F, FreeFormLastName = F, BirthDate = F, StateECFERegisteringPersonMNID = F, IsReceivingInterpreterAssistance = F, StateECFEClassroomParticipationMNID = F, StateECFEEducationBackgroundMNID = F, StateECFEEmploymentStatusMNID = F, HouseholdIncome = F, NumberOfPeopleInHousehold = F, FirstName = F, LastName = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialPrograms", objectName = "ECFEParentGuardianMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ECFEParentGuardianMN
	#'
	#' This function returns a dataframe or json object of an ECFEParentGuardianMN
	#' @param ECFEParentGuardianMNID The ID of the ECFEParentGuardianMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ECFEParentGuardianMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ECFEParentGuardianMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ECFEParentGuardianMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A dataframe or of ECFEParentGuardianMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getECFEParentGuardianMN <- function(ECFEParentGuardianMNID, ECFEProgramRegistrationMNID = F, NameEntryType = F, NameID = F, FreeFormFirstName = F, FreeFormLastName = F, BirthDate = F, StateECFERegisteringPersonMNID = F, IsReceivingInterpreterAssistance = F, StateECFEClassroomParticipationMNID = F, StateECFEEducationBackgroundMNID = F, StateECFEEmploymentStatusMNID = F, HouseholdIncome = F, NumberOfPeopleInHousehold = F, FirstName = F, LastName = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ECFEParentGuardianMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialPrograms", objectName = "ECFEParentGuardianMN", objectId = ECFEParentGuardianMNID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ECFEParentGuardianMN
	#'
	#' This function deletes an ECFEParentGuardianMN
	#' @param ECFEParentGuardianMNID The ID of the ECFEParentGuardianMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The ECFEParentGuardianMNID of the deleted ECFEParentGuardianMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteECFEParentGuardianMN <- function(ECFEParentGuardianMNID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialPrograms", objectName = "ECFEParentGuardianMN", objectId = ECFEParentGuardianMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ECFEParentGuardianMN
	#'
	#' This function creates an ECFEParentGuardianMN
	#' @param fieldNames The field values to give the created ECFEParentGuardianMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A newly created ECFEParentGuardianMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createECFEParentGuardianMN <- function(ECFEProgramRegistrationMNID = NULL, NameEntryType = NULL, NameID = NULL, FreeFormFirstName = NULL, FreeFormLastName = NULL, BirthDate = NULL, StateECFERegisteringPersonMNID = NULL, IsReceivingInterpreterAssistance = NULL, StateECFEClassroomParticipationMNID = NULL, StateECFEEducationBackgroundMNID = NULL, StateECFEEmploymentStatusMNID = NULL, HouseholdIncome = NULL, NumberOfPeopleInHousehold = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialPrograms", objectName = "ECFEParentGuardianMN", body = list(DataObject = body), searchFields = append("ECFEParentGuardianMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ECFEParentGuardianMN
	#'
	#' This function modifies an ECFEParentGuardianMN
	#' @param fieldNames The field values to give the modified ECFEParentGuardianMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The modified ECFEParentGuardianMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyECFEParentGuardianMN <- function(ECFEParentGuardianMNID, ECFEProgramRegistrationMNID = NULL, NameEntryType = NULL, NameID = NULL, FreeFormFirstName = NULL, FreeFormLastName = NULL, BirthDate = NULL, StateECFERegisteringPersonMNID = NULL, IsReceivingInterpreterAssistance = NULL, StateECFEClassroomParticipationMNID = NULL, StateECFEEducationBackgroundMNID = NULL, StateECFEEmploymentStatusMNID = NULL, HouseholdIncome = NULL, NumberOfPeopleInHousehold = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialPrograms", objectName = "ECFEParentGuardianMN", objectId = ECFEParentGuardianMNID, body = list(DataObject = body), searchFields = append("ECFEParentGuardianMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ECFEProgramRegistrationMNS
	#'
	#' This function returns a dataframe or json object of ECFEProgramRegistrationMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ECFEProgramRegistrationMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ECFEProgramRegistrationMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ECFEProgramRegistrationMN') to get more field paths.
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
	#' @concept Special Programs
	#' @return A list of ECFEProgramRegistrationMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listECFEProgramRegistrationMNS <- function(searchConditionsList = NULL, ECFEProgramRegistrationMNID = F, DistrictID = F, SchoolYearID = F, RegistrationDate = F, ProgramType = F, IsScholarship = F, Comment = F, StateECFEProgramNameMNID = F, StateECFEProgramModelMNID = F, StateECFEIEPStatusMNID = F, CountOfClasses = F, HoursAttended = F, DaysAttended = F, DaysAbsent = F, SpecifyOtherPrimaryLanguage = F, StateLanguageCodeMNIDSecondary = F, IsEthnicityUnspecified = F, IsEthnicityOther = F, OtherSpecify = F, StateECFEFeeStatusMNID = F, IsPathways = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialPrograms", objectName = "ECFEProgramRegistrationMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ECFEProgramRegistrationMN
	#'
	#' This function returns a dataframe or json object of an ECFEProgramRegistrationMN
	#' @param ECFEProgramRegistrationMNID The ID of the ECFEProgramRegistrationMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ECFEProgramRegistrationMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ECFEProgramRegistrationMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ECFEProgramRegistrationMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A dataframe or of ECFEProgramRegistrationMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getECFEProgramRegistrationMN <- function(ECFEProgramRegistrationMNID, DistrictID = F, SchoolYearID = F, RegistrationDate = F, ProgramType = F, IsScholarship = F, Comment = F, StateECFEProgramNameMNID = F, StateECFEProgramModelMNID = F, StateECFEIEPStatusMNID = F, CountOfClasses = F, HoursAttended = F, DaysAttended = F, DaysAbsent = F, SpecifyOtherPrimaryLanguage = F, StateLanguageCodeMNIDSecondary = F, IsEthnicityUnspecified = F, IsEthnicityOther = F, OtherSpecify = F, StateECFEFeeStatusMNID = F, IsPathways = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ECFEProgramRegistrationMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialPrograms", objectName = "ECFEProgramRegistrationMN", objectId = ECFEProgramRegistrationMNID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ECFEProgramRegistrationMN
	#'
	#' This function deletes an ECFEProgramRegistrationMN
	#' @param ECFEProgramRegistrationMNID The ID of the ECFEProgramRegistrationMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The ECFEProgramRegistrationMNID of the deleted ECFEProgramRegistrationMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteECFEProgramRegistrationMN <- function(ECFEProgramRegistrationMNID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialPrograms", objectName = "ECFEProgramRegistrationMN", objectId = ECFEProgramRegistrationMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ECFEProgramRegistrationMN
	#'
	#' This function creates an ECFEProgramRegistrationMN
	#' @param fieldNames The field values to give the created ECFEProgramRegistrationMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A newly created ECFEProgramRegistrationMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createECFEProgramRegistrationMN <- function(DistrictID = NULL, SchoolYearID = NULL, RegistrationDate = NULL, ProgramType = NULL, IsScholarship = NULL, Comment = NULL, StateECFEProgramNameMNID = NULL, StateECFEProgramModelMNID = NULL, StateECFEIEPStatusMNID = NULL, CountOfClasses = NULL, HoursAttended = NULL, DaysAttended = NULL, DaysAbsent = NULL, SpecifyOtherPrimaryLanguage = NULL, StateLanguageCodeMNIDSecondary = NULL, IsEthnicityUnspecified = NULL, IsEthnicityOther = NULL, OtherSpecify = NULL, StateECFEFeeStatusMNID = NULL, StudentID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialPrograms", objectName = "ECFEProgramRegistrationMN", body = list(DataObject = body), searchFields = append("ECFEProgramRegistrationMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ECFEProgramRegistrationMN
	#'
	#' This function modifies an ECFEProgramRegistrationMN
	#' @param fieldNames The field values to give the modified ECFEProgramRegistrationMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The modified ECFEProgramRegistrationMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyECFEProgramRegistrationMN <- function(ECFEProgramRegistrationMNID, DistrictID = NULL, SchoolYearID = NULL, RegistrationDate = NULL, ProgramType = NULL, IsScholarship = NULL, Comment = NULL, StateECFEProgramNameMNID = NULL, StateECFEProgramModelMNID = NULL, StateECFEIEPStatusMNID = NULL, CountOfClasses = NULL, HoursAttended = NULL, DaysAttended = NULL, DaysAbsent = NULL, SpecifyOtherPrimaryLanguage = NULL, StateLanguageCodeMNIDSecondary = NULL, IsEthnicityUnspecified = NULL, IsEthnicityOther = NULL, OtherSpecify = NULL, StateECFEFeeStatusMNID = NULL, StudentID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialPrograms", objectName = "ECFEProgramRegistrationMN", objectId = ECFEProgramRegistrationMNID, body = list(DataObject = body), searchFields = append("ECFEProgramRegistrationMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Section504Disabilities
	#'
	#' This function returns a dataframe or json object of Section504Disabilities
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Section504Disabilities. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Section504Disabilities.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Section504Disability') to get more field paths.
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
	#' @concept Special Programs
	#' @return A list of Section504Disabilities
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSection504Disabilities <- function(searchConditionsList = NULL, Section504DisabilityID = F, DistrictID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialPrograms", objectName = "Section504Disability", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Section504Disability
	#'
	#' This function returns a dataframe or json object of a Section504Disability
	#' @param Section504DisabilityID The ID of the Section504Disability to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Section504Disability. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Section504Disability.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Section504Disability') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A dataframe or of Section504Disability
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSection504Disability <- function(Section504DisabilityID, DistrictID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "Section504DisabilityID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialPrograms", objectName = "Section504Disability", objectId = Section504DisabilityID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Section504Disability
	#'
	#' This function deletes a Section504Disability
	#' @param Section504DisabilityID The ID of the Section504Disability to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The Section504DisabilityID of the deleted Section504Disability.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSection504Disability <- function(Section504DisabilityID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialPrograms", objectName = "Section504Disability", objectId = Section504DisabilityID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Section504Disability
	#'
	#' This function creates a Section504Disability
	#' @param fieldNames The field values to give the created Section504Disability. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A newly created Section504Disability
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSection504Disability <- function(DistrictID = NULL, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialPrograms", objectName = "Section504Disability", body = list(DataObject = body), searchFields = append("Section504DisabilityID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Section504Disability
	#'
	#' This function modifies a Section504Disability
	#' @param fieldNames The field values to give the modified Section504Disability. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The modified Section504Disability
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySection504Disability <- function(Section504DisabilityID, DistrictID = NULL, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialPrograms", objectName = "Section504Disability", objectId = Section504DisabilityID, body = list(DataObject = body), searchFields = append("Section504DisabilityID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Section504LifeActivities
	#'
	#' This function returns a dataframe or json object of Section504LifeActivities
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Section504LifeActivities. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Section504LifeActivities.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Section504LifeActivity') to get more field paths.
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
	#' @concept Special Programs
	#' @return A list of Section504LifeActivities
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSection504LifeActivities <- function(searchConditionsList = NULL, Section504LifeActivityID = F, DistrictID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialPrograms", objectName = "Section504LifeActivity", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Section504LifeActivity
	#'
	#' This function returns a dataframe or json object of a Section504LifeActivity
	#' @param Section504LifeActivityID The ID of the Section504LifeActivity to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Section504LifeActivity. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Section504LifeActivity.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Section504LifeActivity') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A dataframe or of Section504LifeActivity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSection504LifeActivity <- function(Section504LifeActivityID, DistrictID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "Section504LifeActivityID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialPrograms", objectName = "Section504LifeActivity", objectId = Section504LifeActivityID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Section504LifeActivity
	#'
	#' This function deletes a Section504LifeActivity
	#' @param Section504LifeActivityID The ID of the Section504LifeActivity to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The Section504LifeActivityID of the deleted Section504LifeActivity.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSection504LifeActivity <- function(Section504LifeActivityID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialPrograms", objectName = "Section504LifeActivity", objectId = Section504LifeActivityID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Section504LifeActivity
	#'
	#' This function creates a Section504LifeActivity
	#' @param fieldNames The field values to give the created Section504LifeActivity. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A newly created Section504LifeActivity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSection504LifeActivity <- function(DistrictID = NULL, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialPrograms", objectName = "Section504LifeActivity", body = list(DataObject = body), searchFields = append("Section504LifeActivityID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Section504LifeActivity
	#'
	#' This function modifies a Section504LifeActivity
	#' @param fieldNames The field values to give the modified Section504LifeActivity. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The modified Section504LifeActivity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySection504LifeActivity <- function(Section504LifeActivityID, DistrictID = NULL, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialPrograms", objectName = "Section504LifeActivity", objectId = Section504LifeActivityID, body = list(DataObject = body), searchFields = append("Section504LifeActivityID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Section504ReferralTypes
	#'
	#' This function returns a dataframe or json object of Section504ReferralTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Section504ReferralTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Section504ReferralTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Section504ReferralType') to get more field paths.
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
	#' @concept Special Programs
	#' @return A list of Section504ReferralTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSection504ReferralTypes <- function(searchConditionsList = NULL, Section504ReferralTypeID = F, DistrictID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialPrograms", objectName = "Section504ReferralType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Section504ReferralType
	#'
	#' This function returns a dataframe or json object of a Section504ReferralType
	#' @param Section504ReferralTypeID The ID of the Section504ReferralType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Section504ReferralType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Section504ReferralType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Section504ReferralType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A dataframe or of Section504ReferralType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSection504ReferralType <- function(Section504ReferralTypeID, DistrictID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "Section504ReferralTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialPrograms", objectName = "Section504ReferralType", objectId = Section504ReferralTypeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Section504ReferralType
	#'
	#' This function deletes a Section504ReferralType
	#' @param Section504ReferralTypeID The ID of the Section504ReferralType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The Section504ReferralTypeID of the deleted Section504ReferralType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSection504ReferralType <- function(Section504ReferralTypeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialPrograms", objectName = "Section504ReferralType", objectId = Section504ReferralTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Section504ReferralType
	#'
	#' This function creates a Section504ReferralType
	#' @param fieldNames The field values to give the created Section504ReferralType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A newly created Section504ReferralType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSection504ReferralType <- function(DistrictID = NULL, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialPrograms", objectName = "Section504ReferralType", body = list(DataObject = body), searchFields = append("Section504ReferralTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Section504ReferralType
	#'
	#' This function modifies a Section504ReferralType
	#' @param fieldNames The field values to give the modified Section504ReferralType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The modified Section504ReferralType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySection504ReferralType <- function(Section504ReferralTypeID, DistrictID = NULL, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialPrograms", objectName = "Section504ReferralType", objectId = Section504ReferralTypeID, body = list(DataObject = body), searchFields = append("Section504ReferralTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CommunityServiceHours
	#'
	#' This function returns a dataframe or json object of CommunityServiceHours
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CommunityServiceHours. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CommunityServiceHours.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CommunityServiceHours') to get more field paths.
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
	#' @concept Special Programs
	#' @return A list of CommunityServiceHours
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCommunityServiceHours <- function(searchConditionsList = NULL, CommunityServiceHoursID = F, DistrictID = F, Date = F, Hours = F, Comment = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialPrograms", objectName = "CommunityServiceHours", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CommunityServiceHours
	#'
	#' This function returns a dataframe or json object of a CommunityServiceHours
	#' @param CommunityServiceHoursID The ID of the CommunityServiceHours to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CommunityServiceHours. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CommunityServiceHours.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CommunityServiceHours') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A dataframe or of CommunityServiceHours
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCommunityServiceHours <- function(CommunityServiceHoursID, DistrictID = F, Date = F, Hours = F, Comment = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CommunityServiceHoursID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialPrograms", objectName = "CommunityServiceHours", objectId = CommunityServiceHoursID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CommunityServiceHours
	#'
	#' This function deletes a CommunityServiceHours
	#' @param CommunityServiceHoursID The ID of the CommunityServiceHours to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The CommunityServiceHoursID of the deleted CommunityServiceHours.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCommunityServiceHours <- function(CommunityServiceHoursID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialPrograms", objectName = "CommunityServiceHours", objectId = CommunityServiceHoursID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CommunityServiceHours
	#'
	#' This function creates a CommunityServiceHours
	#' @param fieldNames The field values to give the created CommunityServiceHours. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A newly created CommunityServiceHours
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCommunityServiceHours <- function(DistrictID = NULL, Date = NULL, Hours = NULL, Comment = NULL, StudentID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialPrograms", objectName = "CommunityServiceHours", body = list(DataObject = body), searchFields = append("CommunityServiceHoursID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CommunityServiceHours
	#'
	#' This function modifies a CommunityServiceHours
	#' @param fieldNames The field values to give the modified CommunityServiceHours. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The modified CommunityServiceHours
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCommunityServiceHours <- function(CommunityServiceHoursID, DistrictID = NULL, Date = NULL, Hours = NULL, Comment = NULL, StudentID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialPrograms", objectName = "CommunityServiceHours", objectId = CommunityServiceHoursID, body = list(DataObject = body), searchFields = append("CommunityServiceHoursID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StateReportingExclusions
	#'
	#' This function returns a dataframe or json object of StateReportingExclusions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StateReportingExclusions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StateReportingExclusions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StateReportingExclusion') to get more field paths.
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
	#' @concept Special Programs
	#' @return A list of StateReportingExclusions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStateReportingExclusions <- function(searchConditionsList = NULL, StateReportingExclusionID = F, DistrictID = F, StateReportingExclusionTypeID = F, EntryComment = F, ExitComment = F, StartDate = F, EndDate = F, IsActive = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialPrograms", objectName = "StateReportingExclusion", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StateReportingExclusion
	#'
	#' This function returns a dataframe or json object of a StateReportingExclusion
	#' @param StateReportingExclusionID The ID of the StateReportingExclusion to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StateReportingExclusion. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StateReportingExclusion.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StateReportingExclusion') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A dataframe or of StateReportingExclusion
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStateReportingExclusion <- function(StateReportingExclusionID, DistrictID = F, StateReportingExclusionTypeID = F, EntryComment = F, ExitComment = F, StartDate = F, EndDate = F, IsActive = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StateReportingExclusionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialPrograms", objectName = "StateReportingExclusion", objectId = StateReportingExclusionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StateReportingExclusion
	#'
	#' This function deletes a StateReportingExclusion
	#' @param StateReportingExclusionID The ID of the StateReportingExclusion to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The StateReportingExclusionID of the deleted StateReportingExclusion.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStateReportingExclusion <- function(StateReportingExclusionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialPrograms", objectName = "StateReportingExclusion", objectId = StateReportingExclusionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StateReportingExclusion
	#'
	#' This function creates a StateReportingExclusion
	#' @param fieldNames The field values to give the created StateReportingExclusion. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A newly created StateReportingExclusion
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStateReportingExclusion <- function(DistrictID = NULL, StateReportingExclusionTypeID = NULL, EntryComment = NULL, ExitComment = NULL, StartDate = NULL, EndDate = NULL, StudentID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialPrograms", objectName = "StateReportingExclusion", body = list(DataObject = body), searchFields = append("StateReportingExclusionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StateReportingExclusion
	#'
	#' This function modifies a StateReportingExclusion
	#' @param fieldNames The field values to give the modified StateReportingExclusion. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The modified StateReportingExclusion
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStateReportingExclusion <- function(StateReportingExclusionID, DistrictID = NULL, StateReportingExclusionTypeID = NULL, EntryComment = NULL, ExitComment = NULL, StartDate = NULL, EndDate = NULL, StudentID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialPrograms", objectName = "StateReportingExclusion", objectId = StateReportingExclusionID, body = list(DataObject = body), searchFields = append("StateReportingExclusionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempMassAddSpecialProgramChildren
	#'
	#' This function returns a dataframe or json object of TempMassAddSpecialProgramChildren
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempMassAddSpecialProgramChildren. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempMassAddSpecialProgramChildren.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempMassAddSpecialProgramChild') to get more field paths.
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
	#' @concept Special Programs
	#' @return A list of TempMassAddSpecialProgramChildren
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempMassAddSpecialProgramChildren <- function(searchConditionsList = NULL, TempMassAddSpecialProgramChildID = F, TempMassAddSpecialProgramIDParent = F, RecordType = F, SpecialProgramIDField = F, CodeIDField = F, CodeIDValues = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialPrograms", objectName = "TempMassAddSpecialProgramChild", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempMassAddSpecialProgramChild
	#'
	#' This function returns a dataframe or json object of a TempMassAddSpecialProgramChild
	#' @param TempMassAddSpecialProgramChildID The ID of the TempMassAddSpecialProgramChild to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempMassAddSpecialProgramChild. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempMassAddSpecialProgramChild.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempMassAddSpecialProgramChild') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A dataframe or of TempMassAddSpecialProgramChild
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempMassAddSpecialProgramChild <- function(TempMassAddSpecialProgramChildID, TempMassAddSpecialProgramIDParent = F, RecordType = F, SpecialProgramIDField = F, CodeIDField = F, CodeIDValues = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempMassAddSpecialProgramChildID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialPrograms", objectName = "TempMassAddSpecialProgramChild", objectId = TempMassAddSpecialProgramChildID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempMassAddSpecialProgramChild
	#'
	#' This function deletes a TempMassAddSpecialProgramChild
	#' @param TempMassAddSpecialProgramChildID The ID of the TempMassAddSpecialProgramChild to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The TempMassAddSpecialProgramChildID of the deleted TempMassAddSpecialProgramChild.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempMassAddSpecialProgramChild <- function(TempMassAddSpecialProgramChildID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialPrograms", objectName = "TempMassAddSpecialProgramChild", objectId = TempMassAddSpecialProgramChildID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempMassAddSpecialProgramChild
	#'
	#' This function creates a TempMassAddSpecialProgramChild
	#' @param fieldNames The field values to give the created TempMassAddSpecialProgramChild. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A newly created TempMassAddSpecialProgramChild
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempMassAddSpecialProgramChild <- function(TempMassAddSpecialProgramIDParent = NULL, RecordType = NULL, SpecialProgramIDField = NULL, CodeIDField = NULL, CodeIDValues = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialPrograms", objectName = "TempMassAddSpecialProgramChild", body = list(DataObject = body), searchFields = append("TempMassAddSpecialProgramChildID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempMassAddSpecialProgramChild
	#'
	#' This function modifies a TempMassAddSpecialProgramChild
	#' @param fieldNames The field values to give the modified TempMassAddSpecialProgramChild. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The modified TempMassAddSpecialProgramChild
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempMassAddSpecialProgramChild <- function(TempMassAddSpecialProgramChildID, TempMassAddSpecialProgramIDParent = NULL, RecordType = NULL, SpecialProgramIDField = NULL, CodeIDField = NULL, CodeIDValues = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialPrograms", objectName = "TempMassAddSpecialProgramChild", objectId = TempMassAddSpecialProgramChildID, body = list(DataObject = body), searchFields = append("TempMassAddSpecialProgramChildID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List MethodOfInstructionOfferings
	#'
	#' This function returns a dataframe or json object of MethodOfInstructionOfferings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given MethodOfInstructionOfferings. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MethodOfInstructionOfferings.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MethodOfInstructionOffering') to get more field paths.
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
	#' @concept Special Programs
	#' @return A list of MethodOfInstructionOfferings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listMethodOfInstructionOfferings <- function(searchConditionsList = NULL, MethodOfInstructionOfferingID = F, DistrictID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialPrograms", objectName = "MethodOfInstructionOffering", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a MethodOfInstructionOffering
	#'
	#' This function returns a dataframe or json object of a MethodOfInstructionOffering
	#' @param MethodOfInstructionOfferingID The ID of the MethodOfInstructionOffering to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given MethodOfInstructionOffering. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MethodOfInstructionOffering.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MethodOfInstructionOffering') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A dataframe or of MethodOfInstructionOffering
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getMethodOfInstructionOffering <- function(MethodOfInstructionOfferingID, DistrictID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "MethodOfInstructionOfferingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialPrograms", objectName = "MethodOfInstructionOffering", objectId = MethodOfInstructionOfferingID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a MethodOfInstructionOffering
	#'
	#' This function deletes a MethodOfInstructionOffering
	#' @param MethodOfInstructionOfferingID The ID of the MethodOfInstructionOffering to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The MethodOfInstructionOfferingID of the deleted MethodOfInstructionOffering.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteMethodOfInstructionOffering <- function(MethodOfInstructionOfferingID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialPrograms", objectName = "MethodOfInstructionOffering", objectId = MethodOfInstructionOfferingID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a MethodOfInstructionOffering
	#'
	#' This function creates a MethodOfInstructionOffering
	#' @param fieldNames The field values to give the created MethodOfInstructionOffering. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A newly created MethodOfInstructionOffering
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createMethodOfInstructionOffering <- function(DistrictID = NULL, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialPrograms", objectName = "MethodOfInstructionOffering", body = list(DataObject = body), searchFields = append("MethodOfInstructionOfferingID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a MethodOfInstructionOffering
	#'
	#' This function modifies a MethodOfInstructionOffering
	#' @param fieldNames The field values to give the modified MethodOfInstructionOffering. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The modified MethodOfInstructionOffering
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyMethodOfInstructionOffering <- function(MethodOfInstructionOfferingID, DistrictID = NULL, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialPrograms", objectName = "MethodOfInstructionOffering", objectId = MethodOfInstructionOfferingID, body = list(DataObject = body), searchFields = append("MethodOfInstructionOfferingID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List MethodOfInstructions
	#'
	#' This function returns a dataframe or json object of MethodOfInstructions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given MethodOfInstructions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MethodOfInstructions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MethodOfInstruction') to get more field paths.
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
	#' @concept Special Programs
	#' @return A list of MethodOfInstructions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listMethodOfInstructions <- function(searchConditionsList = NULL, MethodOfInstructionID = F, DistrictID = F, MethodOfInstructionOfferingID = F, EntryComment = F, ExitComment = F, StartDate = F, EndDate = F, IsActive = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialPrograms", objectName = "MethodOfInstruction", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a MethodOfInstruction
	#'
	#' This function returns a dataframe or json object of a MethodOfInstruction
	#' @param MethodOfInstructionID The ID of the MethodOfInstruction to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given MethodOfInstruction. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MethodOfInstruction.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MethodOfInstruction') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A dataframe or of MethodOfInstruction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getMethodOfInstruction <- function(MethodOfInstructionID, DistrictID = F, MethodOfInstructionOfferingID = F, EntryComment = F, ExitComment = F, StartDate = F, EndDate = F, IsActive = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "MethodOfInstructionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialPrograms", objectName = "MethodOfInstruction", objectId = MethodOfInstructionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a MethodOfInstruction
	#'
	#' This function deletes a MethodOfInstruction
	#' @param MethodOfInstructionID The ID of the MethodOfInstruction to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The MethodOfInstructionID of the deleted MethodOfInstruction.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteMethodOfInstruction <- function(MethodOfInstructionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialPrograms", objectName = "MethodOfInstruction", objectId = MethodOfInstructionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a MethodOfInstruction
	#'
	#' This function creates a MethodOfInstruction
	#' @param fieldNames The field values to give the created MethodOfInstruction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return A newly created MethodOfInstruction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createMethodOfInstruction <- function(DistrictID = NULL, MethodOfInstructionOfferingID = NULL, EntryComment = NULL, ExitComment = NULL, StartDate = NULL, EndDate = NULL, StudentID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialPrograms", objectName = "MethodOfInstruction", body = list(DataObject = body), searchFields = append("MethodOfInstructionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a MethodOfInstruction
	#'
	#' This function modifies a MethodOfInstruction
	#' @param fieldNames The field values to give the modified MethodOfInstruction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Special Programs
	#' @return The modified MethodOfInstruction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyMethodOfInstruction <- function(MethodOfInstructionID, DistrictID = NULL, MethodOfInstructionOfferingID = NULL, EntryComment = NULL, ExitComment = NULL, StartDate = NULL, EndDate = NULL, StudentID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialPrograms", objectName = "MethodOfInstruction", objectId = MethodOfInstructionID, body = list(DataObject = body), searchFields = append("MethodOfInstructionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}
