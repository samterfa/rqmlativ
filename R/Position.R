
	#' List AssignmentTypes
	#'
	#' This function returns a dataframe or json object of AssignmentTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssignmentTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssignmentTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssignmentType') to get more field paths.
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
	#' @concept Position
	#' @return A list of AssignmentTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAssignmentTypes <- function(searchConditionsList = NULL, AssignmentTypeMNID = F, StateSTARAssignmentCodeMNID = F, AssignmentTypeID = F, Code = F, Description = F, DistrictID = F, FiscalYearID = F, CodeDescription = F, AssignmentTypeIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalCRDCStaffTypeID = F, FederalEEOCJobCategoryID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "AssignmentType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AssignmentType
	#'
	#' This function returns a dataframe or json object of an AssignmentType
	#' @param AssignmentTypeID The ID of the AssignmentType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssignmentType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssignmentType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssignmentType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of AssignmentType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAssignmentType <- function(AssignmentTypeID, AssignmentTypeMNID = F, StateSTARAssignmentCodeMNID = F, Code = F, Description = F, DistrictID = F, FiscalYearID = F, CodeDescription = F, AssignmentTypeIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalCRDCStaffTypeID = F, FederalEEOCJobCategoryID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AssignmentTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "AssignmentType", objectId = AssignmentTypeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AssignmentType
	#'
	#' This function deletes an AssignmentType
	#' @param AssignmentTypeID The ID of the AssignmentType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The AssignmentTypeID of the deleted AssignmentType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAssignmentType <- function(AssignmentTypeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "AssignmentType", objectId = AssignmentTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AssignmentType
	#'
	#' This function creates an AssignmentType
	#' @param fieldNames The field values to give the created AssignmentType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created AssignmentType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAssignmentType <- function(StateSTARAssignmentCodeMNID = NULL, Code = NULL, Description = NULL, DistrictID = NULL, FiscalYearID = NULL, AssignmentTypeIDClonedFrom = NULL, FederalCRDCStaffTypeID = NULL, FederalEEOCJobCategoryID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "AssignmentType", body = list(DataObject = body), searchFields = append("AssignmentTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AssignmentType
	#'
	#' This function modifies an AssignmentType
	#' @param fieldNames The field values to give the modified AssignmentType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified AssignmentType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAssignmentType <- function(AssignmentTypeID, StateSTARAssignmentCodeMNID = NULL, Code = NULL, Description = NULL, DistrictID = NULL, FiscalYearID = NULL, AssignmentTypeIDClonedFrom = NULL, FederalCRDCStaffTypeID = NULL, FederalEEOCJobCategoryID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "AssignmentType", objectId = AssignmentTypeID, body = list(DataObject = body), searchFields = append("AssignmentTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StateSTARAssignmentCodeMNS
	#'
	#' This function returns a dataframe or json object of StateSTARAssignmentCodeMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StateSTARAssignmentCodeMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StateSTARAssignmentCodeMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StateSTARAssignmentCodeMN') to get more field paths.
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
	#' @concept Position
	#' @return A list of StateSTARAssignmentCodeMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStateSTARAssignmentCodeMNS <- function(searchConditionsList = NULL, StateSTARAssignmentCodeMNID = F, IsLicensedAssignment = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "StateSTARAssignmentCodeMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StateSTARAssignmentCodeMN
	#'
	#' This function returns a dataframe or json object of a StateSTARAssignmentCodeMN
	#' @param StateSTARAssignmentCodeMNID The ID of the StateSTARAssignmentCodeMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StateSTARAssignmentCodeMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StateSTARAssignmentCodeMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StateSTARAssignmentCodeMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of StateSTARAssignmentCodeMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStateSTARAssignmentCodeMN <- function(StateSTARAssignmentCodeMNID, IsLicensedAssignment = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StateSTARAssignmentCodeMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "StateSTARAssignmentCodeMN", objectId = StateSTARAssignmentCodeMNID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StateSTARAssignmentCodeMN
	#'
	#' This function deletes a StateSTARAssignmentCodeMN
	#' @param StateSTARAssignmentCodeMNID The ID of the StateSTARAssignmentCodeMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The StateSTARAssignmentCodeMNID of the deleted StateSTARAssignmentCodeMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStateSTARAssignmentCodeMN <- function(StateSTARAssignmentCodeMNID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "StateSTARAssignmentCodeMN", objectId = StateSTARAssignmentCodeMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StateSTARAssignmentCodeMN
	#'
	#' This function creates a StateSTARAssignmentCodeMN
	#' @param fieldNames The field values to give the created StateSTARAssignmentCodeMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created StateSTARAssignmentCodeMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStateSTARAssignmentCodeMN <- function(IsLicensedAssignment = NULL, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "StateSTARAssignmentCodeMN", body = list(DataObject = body), searchFields = append("StateSTARAssignmentCodeMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StateSTARAssignmentCodeMN
	#'
	#' This function modifies a StateSTARAssignmentCodeMN
	#' @param fieldNames The field values to give the modified StateSTARAssignmentCodeMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified StateSTARAssignmentCodeMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStateSTARAssignmentCodeMN <- function(StateSTARAssignmentCodeMNID, IsLicensedAssignment = NULL, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "StateSTARAssignmentCodeMN", objectId = StateSTARAssignmentCodeMNID, body = list(DataObject = body), searchFields = append("StateSTARAssignmentCodeMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StateSTARHighestEducationLevelMNS
	#'
	#' This function returns a dataframe or json object of StateSTARHighestEducationLevelMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StateSTARHighestEducationLevelMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StateSTARHighestEducationLevelMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StateSTARHighestEducationLevelMN') to get more field paths.
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
	#' @concept Position
	#' @return A list of StateSTARHighestEducationLevelMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStateSTARHighestEducationLevelMNS <- function(searchConditionsList = NULL, StateSTARHighestEducationLevelMNID = F, Code = F, Description = F, CodeDescription = F, BaseCode = F, MinimumAdditionalCreditsThreshold = F, MaximumAdditionalCreditsThreshold = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "StateSTARHighestEducationLevelMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StateSTARHighestEducationLevelMN
	#'
	#' This function returns a dataframe or json object of a StateSTARHighestEducationLevelMN
	#' @param StateSTARHighestEducationLevelMNID The ID of the StateSTARHighestEducationLevelMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StateSTARHighestEducationLevelMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StateSTARHighestEducationLevelMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StateSTARHighestEducationLevelMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of StateSTARHighestEducationLevelMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStateSTARHighestEducationLevelMN <- function(StateSTARHighestEducationLevelMNID, Code = F, Description = F, CodeDescription = F, BaseCode = F, MinimumAdditionalCreditsThreshold = F, MaximumAdditionalCreditsThreshold = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StateSTARHighestEducationLevelMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "StateSTARHighestEducationLevelMN", objectId = StateSTARHighestEducationLevelMNID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StateSTARHighestEducationLevelMN
	#'
	#' This function deletes a StateSTARHighestEducationLevelMN
	#' @param StateSTARHighestEducationLevelMNID The ID of the StateSTARHighestEducationLevelMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The StateSTARHighestEducationLevelMNID of the deleted StateSTARHighestEducationLevelMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStateSTARHighestEducationLevelMN <- function(StateSTARHighestEducationLevelMNID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "StateSTARHighestEducationLevelMN", objectId = StateSTARHighestEducationLevelMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StateSTARHighestEducationLevelMN
	#'
	#' This function creates a StateSTARHighestEducationLevelMN
	#' @param fieldNames The field values to give the created StateSTARHighestEducationLevelMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created StateSTARHighestEducationLevelMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStateSTARHighestEducationLevelMN <- function(Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "StateSTARHighestEducationLevelMN", body = list(DataObject = body), searchFields = append("StateSTARHighestEducationLevelMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StateSTARHighestEducationLevelMN
	#'
	#' This function modifies a StateSTARHighestEducationLevelMN
	#' @param fieldNames The field values to give the modified StateSTARHighestEducationLevelMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified StateSTARHighestEducationLevelMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStateSTARHighestEducationLevelMN <- function(StateSTARHighestEducationLevelMNID, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "StateSTARHighestEducationLevelMN", objectId = StateSTARHighestEducationLevelMNID, body = list(DataObject = body), searchFields = append("StateSTARHighestEducationLevelMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StateReportingDistributions
	#'
	#' This function returns a dataframe or json object of StateReportingDistributions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StateReportingDistributions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StateReportingDistributions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StateReportingDistribution') to get more field paths.
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
	#' @concept Position
	#' @return A list of StateReportingDistributions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStateReportingDistributions <- function(searchConditionsList = NULL, StateReportingDistributionMNID = F, StateSTARGradeLevelMNID = F, StateSTARModeOfTeachingMNID = F, STARPeriodsPerWeek = F, STARLengthOfPeriod = F, STAROutOfDistrictAssignment = F, StateReportingDistributionID = F, AssignmentID = F, PositionTypeID = F, AssignmentTypeID = F, BuildingID = F, Percentage = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StateFTE = F, StateFTEOverride = F, PositionIdentifier = F, ReportedStateFTE = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "StateReportingDistribution", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StateReportingDistribution
	#'
	#' This function returns a dataframe or json object of a StateReportingDistribution
	#' @param StateReportingDistributionID The ID of the StateReportingDistribution to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StateReportingDistribution. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StateReportingDistribution.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StateReportingDistribution') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of StateReportingDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStateReportingDistribution <- function(StateReportingDistributionID, StateReportingDistributionMNID = F, StateSTARGradeLevelMNID = F, StateSTARModeOfTeachingMNID = F, STARPeriodsPerWeek = F, STARLengthOfPeriod = F, STAROutOfDistrictAssignment = F, AssignmentID = F, PositionTypeID = F, AssignmentTypeID = F, BuildingID = F, Percentage = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StateFTE = F, StateFTEOverride = F, PositionIdentifier = F, ReportedStateFTE = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StateReportingDistributionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "StateReportingDistribution", objectId = StateReportingDistributionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StateReportingDistribution
	#'
	#' This function deletes a StateReportingDistribution
	#' @param StateReportingDistributionID The ID of the StateReportingDistribution to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The StateReportingDistributionID of the deleted StateReportingDistribution.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStateReportingDistribution <- function(StateReportingDistributionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "StateReportingDistribution", objectId = StateReportingDistributionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StateReportingDistribution
	#'
	#' This function creates a StateReportingDistribution
	#' @param fieldNames The field values to give the created StateReportingDistribution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created StateReportingDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStateReportingDistribution <- function(StateSTARGradeLevelMNID = NULL, StateSTARModeOfTeachingMNID = NULL, STARPeriodsPerWeek = NULL, STARLengthOfPeriod = NULL, STAROutOfDistrictAssignment = NULL, AssignmentID = NULL, PositionTypeID = NULL, AssignmentTypeID = NULL, BuildingID = NULL, Percentage = NULL, StateFTEOverride = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "StateReportingDistribution", body = list(DataObject = body), searchFields = append("StateReportingDistributionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StateReportingDistribution
	#'
	#' This function modifies a StateReportingDistribution
	#' @param fieldNames The field values to give the modified StateReportingDistribution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified StateReportingDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStateReportingDistribution <- function(StateReportingDistributionID, StateSTARGradeLevelMNID = NULL, StateSTARModeOfTeachingMNID = NULL, STARPeriodsPerWeek = NULL, STARLengthOfPeriod = NULL, STAROutOfDistrictAssignment = NULL, AssignmentID = NULL, PositionTypeID = NULL, AssignmentTypeID = NULL, BuildingID = NULL, Percentage = NULL, StateFTEOverride = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "StateReportingDistribution", objectId = StateReportingDistributionID, body = list(DataObject = body), searchFields = append("StateReportingDistributionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StateSTARModeOfTeachingMNS
	#'
	#' This function returns a dataframe or json object of StateSTARModeOfTeachingMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StateSTARModeOfTeachingMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StateSTARModeOfTeachingMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StateSTARModeOfTeachingMN') to get more field paths.
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
	#' @concept Position
	#' @return A list of StateSTARModeOfTeachingMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStateSTARModeOfTeachingMNS <- function(searchConditionsList = NULL, StateSTARModeOfTeachingMNID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "StateSTARModeOfTeachingMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StateSTARModeOfTeachingMN
	#'
	#' This function returns a dataframe or json object of a StateSTARModeOfTeachingMN
	#' @param StateSTARModeOfTeachingMNID The ID of the StateSTARModeOfTeachingMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StateSTARModeOfTeachingMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StateSTARModeOfTeachingMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StateSTARModeOfTeachingMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of StateSTARModeOfTeachingMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStateSTARModeOfTeachingMN <- function(StateSTARModeOfTeachingMNID, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StateSTARModeOfTeachingMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "StateSTARModeOfTeachingMN", objectId = StateSTARModeOfTeachingMNID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StateSTARModeOfTeachingMN
	#'
	#' This function deletes a StateSTARModeOfTeachingMN
	#' @param StateSTARModeOfTeachingMNID The ID of the StateSTARModeOfTeachingMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The StateSTARModeOfTeachingMNID of the deleted StateSTARModeOfTeachingMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStateSTARModeOfTeachingMN <- function(StateSTARModeOfTeachingMNID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "StateSTARModeOfTeachingMN", objectId = StateSTARModeOfTeachingMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StateSTARModeOfTeachingMN
	#'
	#' This function creates a StateSTARModeOfTeachingMN
	#' @param fieldNames The field values to give the created StateSTARModeOfTeachingMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created StateSTARModeOfTeachingMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStateSTARModeOfTeachingMN <- function(Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "StateSTARModeOfTeachingMN", body = list(DataObject = body), searchFields = append("StateSTARModeOfTeachingMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StateSTARModeOfTeachingMN
	#'
	#' This function modifies a StateSTARModeOfTeachingMN
	#' @param fieldNames The field values to give the modified StateSTARModeOfTeachingMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified StateSTARModeOfTeachingMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStateSTARModeOfTeachingMN <- function(StateSTARModeOfTeachingMNID, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "StateSTARModeOfTeachingMN", objectId = StateSTARModeOfTeachingMNID, body = list(DataObject = body), searchFields = append("StateSTARModeOfTeachingMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StateSTARGradeLevelMNS
	#'
	#' This function returns a dataframe or json object of StateSTARGradeLevelMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StateSTARGradeLevelMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StateSTARGradeLevelMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StateSTARGradeLevelMN') to get more field paths.
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
	#' @concept Position
	#' @return A list of StateSTARGradeLevelMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStateSTARGradeLevelMNS <- function(searchConditionsList = NULL, StateSTARGradeLevelMNID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsIndividualGradeLevel = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "StateSTARGradeLevelMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StateSTARGradeLevelMN
	#'
	#' This function returns a dataframe or json object of a StateSTARGradeLevelMN
	#' @param StateSTARGradeLevelMNID The ID of the StateSTARGradeLevelMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StateSTARGradeLevelMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StateSTARGradeLevelMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StateSTARGradeLevelMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of StateSTARGradeLevelMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStateSTARGradeLevelMN <- function(StateSTARGradeLevelMNID, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsIndividualGradeLevel = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StateSTARGradeLevelMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "StateSTARGradeLevelMN", objectId = StateSTARGradeLevelMNID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StateSTARGradeLevelMN
	#'
	#' This function deletes a StateSTARGradeLevelMN
	#' @param StateSTARGradeLevelMNID The ID of the StateSTARGradeLevelMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The StateSTARGradeLevelMNID of the deleted StateSTARGradeLevelMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStateSTARGradeLevelMN <- function(StateSTARGradeLevelMNID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "StateSTARGradeLevelMN", objectId = StateSTARGradeLevelMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StateSTARGradeLevelMN
	#'
	#' This function creates a StateSTARGradeLevelMN
	#' @param fieldNames The field values to give the created StateSTARGradeLevelMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created StateSTARGradeLevelMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStateSTARGradeLevelMN <- function(Code = NULL, Description = NULL, IsIndividualGradeLevel = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "StateSTARGradeLevelMN", body = list(DataObject = body), searchFields = append("StateSTARGradeLevelMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StateSTARGradeLevelMN
	#'
	#' This function modifies a StateSTARGradeLevelMN
	#' @param fieldNames The field values to give the modified StateSTARGradeLevelMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified StateSTARGradeLevelMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStateSTARGradeLevelMN <- function(StateSTARGradeLevelMNID, Code = NULL, Description = NULL, IsIndividualGradeLevel = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "StateSTARGradeLevelMN", objectId = StateSTARGradeLevelMNID, body = list(DataObject = body), searchFields = append("StateSTARGradeLevelMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempAssignmentDetails
	#'
	#' This function returns a dataframe or json object of TempAssignmentDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAssignmentDetails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAssignmentDetails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAssignmentDetail') to get more field paths.
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
	#' @concept Position
	#' @return A list of TempAssignmentDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempAssignmentDetails <- function(searchConditionsList = NULL, TempAssignmentDetailID = F, Employee = F, OldAnnualizedPay = F, NewAnnualizedPay = F, OldTotalPay = F, NewTotalPay = F, OldHourlyPay = F, NewHourlyPay = F, OldDailyPay = F, NewDailyPay = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartDate = F, EndDate = F, OriginalAssignmentDetailID = F, OldEnteredRate = F, NewEnteredRate = F, EmployeeNumber = F, AssignmentID = F, EnteredFTE = F, SecondsPerDay = F, MatrixID = F, OldMatrixCode = F, NewMatrixCode = F, PositionTypeCode = F, PositionTypeDescription = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, OldSupplementTotalPay = F, NewSupplementTotalPay = F, ErrorCount = F, IsPrimary = F, OldEnteredFTE = F, OldSecondsPerDay = F, OldActivePaidDayCount = F, NewActivePaidDayCount = F, OldWorkdayCount = F, NewWorkdayCount = F, PositionID = F, CreatedFromPositionChange = F, OldCalendarSeconds = F, OldFormattedCalendarSeconds = F, NewCalendarSeconds = F, NewFormattedCalendarSeconds = F, OldPaidFullDays = F, NewPaidFullDays = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempAssignmentDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempAssignmentDetail
	#'
	#' This function returns a dataframe or json object of a TempAssignmentDetail
	#' @param TempAssignmentDetailID The ID of the TempAssignmentDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAssignmentDetail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAssignmentDetail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAssignmentDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of TempAssignmentDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempAssignmentDetail <- function(TempAssignmentDetailID, Employee = F, OldAnnualizedPay = F, NewAnnualizedPay = F, OldTotalPay = F, NewTotalPay = F, OldHourlyPay = F, NewHourlyPay = F, OldDailyPay = F, NewDailyPay = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartDate = F, EndDate = F, OriginalAssignmentDetailID = F, OldEnteredRate = F, NewEnteredRate = F, EmployeeNumber = F, AssignmentID = F, EnteredFTE = F, SecondsPerDay = F, MatrixID = F, OldMatrixCode = F, NewMatrixCode = F, PositionTypeCode = F, PositionTypeDescription = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, OldSupplementTotalPay = F, NewSupplementTotalPay = F, ErrorCount = F, IsPrimary = F, OldEnteredFTE = F, OldSecondsPerDay = F, OldActivePaidDayCount = F, NewActivePaidDayCount = F, OldWorkdayCount = F, NewWorkdayCount = F, PositionID = F, CreatedFromPositionChange = F, OldCalendarSeconds = F, OldFormattedCalendarSeconds = F, NewCalendarSeconds = F, NewFormattedCalendarSeconds = F, OldPaidFullDays = F, NewPaidFullDays = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempAssignmentDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "TempAssignmentDetail", objectId = TempAssignmentDetailID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempAssignmentDetail
	#'
	#' This function deletes a TempAssignmentDetail
	#' @param TempAssignmentDetailID The ID of the TempAssignmentDetail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The TempAssignmentDetailID of the deleted TempAssignmentDetail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempAssignmentDetail <- function(TempAssignmentDetailID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "TempAssignmentDetail", objectId = TempAssignmentDetailID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempAssignmentDetail
	#'
	#' This function creates a TempAssignmentDetail
	#' @param fieldNames The field values to give the created TempAssignmentDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created TempAssignmentDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempAssignmentDetail <- function(Employee = NULL, OldAnnualizedPay = NULL, NewAnnualizedPay = NULL, OldTotalPay = NULL, NewTotalPay = NULL, OldHourlyPay = NULL, NewHourlyPay = NULL, OldDailyPay = NULL, NewDailyPay = NULL, StartDate = NULL, EndDate = NULL, OriginalAssignmentDetailID = NULL, OldEnteredRate = NULL, NewEnteredRate = NULL, EmployeeNumber = NULL, AssignmentID = NULL, EnteredFTE = NULL, SecondsPerDay = NULL, MatrixID = NULL, OldMatrixCode = NULL, NewMatrixCode = NULL, PositionTypeCode = NULL, PositionTypeDescription = NULL, PositionDistributionsAssignmentTypeCodes = NULL, PositionDistributionsAssignmentTypeDescriptions = NULL, PositionDistributionsBuildingCodes = NULL, PositionDistributionsBuildingDescriptions = NULL, OldSupplementTotalPay = NULL, NewSupplementTotalPay = NULL, ErrorCount = NULL, IsPrimary = NULL, OldEnteredFTE = NULL, OldSecondsPerDay = NULL, OldActivePaidDayCount = NULL, NewActivePaidDayCount = NULL, OldWorkdayCount = NULL, NewWorkdayCount = NULL, PositionID = NULL, CreatedFromPositionChange = NULL, OldCalendarSeconds = NULL, NewCalendarSeconds = NULL, OldPaidFullDays = NULL, NewPaidFullDays = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "TempAssignmentDetail", body = list(DataObject = body), searchFields = append("TempAssignmentDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempAssignmentDetail
	#'
	#' This function modifies a TempAssignmentDetail
	#' @param fieldNames The field values to give the modified TempAssignmentDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified TempAssignmentDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempAssignmentDetail <- function(TempAssignmentDetailID, Employee = NULL, OldAnnualizedPay = NULL, NewAnnualizedPay = NULL, OldTotalPay = NULL, NewTotalPay = NULL, OldHourlyPay = NULL, NewHourlyPay = NULL, OldDailyPay = NULL, NewDailyPay = NULL, StartDate = NULL, EndDate = NULL, OriginalAssignmentDetailID = NULL, OldEnteredRate = NULL, NewEnteredRate = NULL, EmployeeNumber = NULL, AssignmentID = NULL, EnteredFTE = NULL, SecondsPerDay = NULL, MatrixID = NULL, OldMatrixCode = NULL, NewMatrixCode = NULL, PositionTypeCode = NULL, PositionTypeDescription = NULL, PositionDistributionsAssignmentTypeCodes = NULL, PositionDistributionsAssignmentTypeDescriptions = NULL, PositionDistributionsBuildingCodes = NULL, PositionDistributionsBuildingDescriptions = NULL, OldSupplementTotalPay = NULL, NewSupplementTotalPay = NULL, ErrorCount = NULL, IsPrimary = NULL, OldEnteredFTE = NULL, OldSecondsPerDay = NULL, OldActivePaidDayCount = NULL, NewActivePaidDayCount = NULL, OldWorkdayCount = NULL, NewWorkdayCount = NULL, PositionID = NULL, CreatedFromPositionChange = NULL, OldCalendarSeconds = NULL, NewCalendarSeconds = NULL, OldPaidFullDays = NULL, NewPaidFullDays = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "TempAssignmentDetail", objectId = TempAssignmentDetailID, body = list(DataObject = body), searchFields = append("TempAssignmentDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempEmployeePlacementDetails
	#'
	#' This function returns a dataframe or json object of TempEmployeePlacementDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempEmployeePlacementDetails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempEmployeePlacementDetails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempEmployeePlacementDetail') to get more field paths.
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
	#' @concept Position
	#' @return A list of TempEmployeePlacementDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempEmployeePlacementDetails <- function(searchConditionsList = NULL, TempEmployeePlacementDetailID = F, Employee = F, OldStep = F, NewStep = F, Credits = F, OldLane = F, OldLaneID = F, EmployeePlacementID = F, Placement = F, EffectiveDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeNumber = F, TempType = F, EmployeePlacementDetailID = F, NewLane = F, NewLaneID = F, Message = F, EmployeeID = F, NewCredits = F, PlacementID = F, EmployeePlacementDescription = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempEmployeePlacementDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempEmployeePlacementDetail
	#'
	#' This function returns a dataframe or json object of a TempEmployeePlacementDetail
	#' @param TempEmployeePlacementDetailID The ID of the TempEmployeePlacementDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempEmployeePlacementDetail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempEmployeePlacementDetail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempEmployeePlacementDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of TempEmployeePlacementDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempEmployeePlacementDetail <- function(TempEmployeePlacementDetailID, Employee = F, OldStep = F, NewStep = F, Credits = F, OldLane = F, OldLaneID = F, EmployeePlacementID = F, Placement = F, EffectiveDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeNumber = F, TempType = F, EmployeePlacementDetailID = F, NewLane = F, NewLaneID = F, Message = F, EmployeeID = F, NewCredits = F, PlacementID = F, EmployeePlacementDescription = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempEmployeePlacementDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "TempEmployeePlacementDetail", objectId = TempEmployeePlacementDetailID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempEmployeePlacementDetail
	#'
	#' This function deletes a TempEmployeePlacementDetail
	#' @param TempEmployeePlacementDetailID The ID of the TempEmployeePlacementDetail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The TempEmployeePlacementDetailID of the deleted TempEmployeePlacementDetail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempEmployeePlacementDetail <- function(TempEmployeePlacementDetailID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "TempEmployeePlacementDetail", objectId = TempEmployeePlacementDetailID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempEmployeePlacementDetail
	#'
	#' This function creates a TempEmployeePlacementDetail
	#' @param fieldNames The field values to give the created TempEmployeePlacementDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created TempEmployeePlacementDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempEmployeePlacementDetail <- function(Employee = NULL, OldStep = NULL, NewStep = NULL, Credits = NULL, OldLane = NULL, OldLaneID = NULL, EmployeePlacementID = NULL, Placement = NULL, EffectiveDate = NULL, EmployeeNumber = NULL, TempType = NULL, EmployeePlacementDetailID = NULL, NewLane = NULL, NewLaneID = NULL, Message = NULL, EmployeeID = NULL, NewCredits = NULL, PlacementID = NULL, EmployeePlacementDescription = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "TempEmployeePlacementDetail", body = list(DataObject = body), searchFields = append("TempEmployeePlacementDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempEmployeePlacementDetail
	#'
	#' This function modifies a TempEmployeePlacementDetail
	#' @param fieldNames The field values to give the modified TempEmployeePlacementDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified TempEmployeePlacementDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempEmployeePlacementDetail <- function(TempEmployeePlacementDetailID, Employee = NULL, OldStep = NULL, NewStep = NULL, Credits = NULL, OldLane = NULL, OldLaneID = NULL, EmployeePlacementID = NULL, Placement = NULL, EffectiveDate = NULL, EmployeeNumber = NULL, TempType = NULL, EmployeePlacementDetailID = NULL, NewLane = NULL, NewLaneID = NULL, Message = NULL, EmployeeID = NULL, NewCredits = NULL, PlacementID = NULL, EmployeePlacementDescription = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "TempEmployeePlacementDetail", objectId = TempEmployeePlacementDetailID, body = list(DataObject = body), searchFields = append("TempEmployeePlacementDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempOrganizationChartRelationships
	#'
	#' This function returns a dataframe or json object of TempOrganizationChartRelationships
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempOrganizationChartRelationships. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempOrganizationChartRelationships.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempOrganizationChartRelationship') to get more field paths.
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
	#' @concept Position
	#' @return A list of TempOrganizationChartRelationships
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempOrganizationChartRelationships <- function(searchConditionsList = NULL, TempOrganizationChartRelationshipID = F, OrganizationChartID = F, PositionIDSupervisor = F, PositionID = F, PositionPositionTypeDescription = F, PositionPositionNumberCode = F, PositionClosingAssignmentEmployee = F, PositionCurrentAssignmentEmployee = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PositionClosingAssignmentEmployeeNumber = F, PositionCurrentAssignmentEmployeeNumber = F, PositionPositionTypeCode = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempOrganizationChartRelationship", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempOrganizationChartRelationship
	#'
	#' This function returns a dataframe or json object of a TempOrganizationChartRelationship
	#' @param TempOrganizationChartRelationshipID The ID of the TempOrganizationChartRelationship to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempOrganizationChartRelationship. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempOrganizationChartRelationship.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempOrganizationChartRelationship') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of TempOrganizationChartRelationship
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempOrganizationChartRelationship <- function(TempOrganizationChartRelationshipID, OrganizationChartID = F, PositionIDSupervisor = F, PositionID = F, PositionPositionTypeDescription = F, PositionPositionNumberCode = F, PositionClosingAssignmentEmployee = F, PositionCurrentAssignmentEmployee = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PositionClosingAssignmentEmployeeNumber = F, PositionCurrentAssignmentEmployeeNumber = F, PositionPositionTypeCode = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempOrganizationChartRelationshipID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "TempOrganizationChartRelationship", objectId = TempOrganizationChartRelationshipID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempOrganizationChartRelationship
	#'
	#' This function deletes a TempOrganizationChartRelationship
	#' @param TempOrganizationChartRelationshipID The ID of the TempOrganizationChartRelationship to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The TempOrganizationChartRelationshipID of the deleted TempOrganizationChartRelationship.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempOrganizationChartRelationship <- function(TempOrganizationChartRelationshipID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "TempOrganizationChartRelationship", objectId = TempOrganizationChartRelationshipID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempOrganizationChartRelationship
	#'
	#' This function creates a TempOrganizationChartRelationship
	#' @param fieldNames The field values to give the created TempOrganizationChartRelationship. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created TempOrganizationChartRelationship
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempOrganizationChartRelationship <- function(OrganizationChartID = NULL, PositionIDSupervisor = NULL, PositionID = NULL, PositionPositionTypeDescription = NULL, PositionPositionNumberCode = NULL, PositionClosingAssignmentEmployee = NULL, PositionCurrentAssignmentEmployee = NULL, PositionClosingAssignmentEmployeeNumber = NULL, PositionCurrentAssignmentEmployeeNumber = NULL, PositionPositionTypeCode = NULL, PositionDistributionsAssignmentTypeCodes = NULL, PositionDistributionsAssignmentTypeDescriptions = NULL, PositionDistributionsBuildingCodes = NULL, PositionDistributionsBuildingDescriptions = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "TempOrganizationChartRelationship", body = list(DataObject = body), searchFields = append("TempOrganizationChartRelationshipID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempOrganizationChartRelationship
	#'
	#' This function modifies a TempOrganizationChartRelationship
	#' @param fieldNames The field values to give the modified TempOrganizationChartRelationship. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified TempOrganizationChartRelationship
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempOrganizationChartRelationship <- function(TempOrganizationChartRelationshipID, OrganizationChartID = NULL, PositionIDSupervisor = NULL, PositionID = NULL, PositionPositionTypeDescription = NULL, PositionPositionNumberCode = NULL, PositionClosingAssignmentEmployee = NULL, PositionCurrentAssignmentEmployee = NULL, PositionClosingAssignmentEmployeeNumber = NULL, PositionCurrentAssignmentEmployeeNumber = NULL, PositionPositionTypeCode = NULL, PositionDistributionsAssignmentTypeCodes = NULL, PositionDistributionsAssignmentTypeDescriptions = NULL, PositionDistributionsBuildingCodes = NULL, PositionDistributionsBuildingDescriptions = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "TempOrganizationChartRelationship", objectId = TempOrganizationChartRelationshipID, body = list(DataObject = body), searchFields = append("TempOrganizationChartRelationshipID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AssignmentSchedules
	#'
	#' This function returns a dataframe or json object of AssignmentSchedules
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssignmentSchedules. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssignmentSchedules.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssignmentSchedule') to get more field paths.
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
	#' @concept Position
	#' @return A list of AssignmentSchedules
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAssignmentSchedules <- function(searchConditionsList = NULL, AssignmentScheduleID = F, AssignmentID = F, StartDate = F, EndDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "AssignmentSchedule", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AssignmentSchedule
	#'
	#' This function returns a dataframe or json object of an AssignmentSchedule
	#' @param AssignmentScheduleID The ID of the AssignmentSchedule to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssignmentSchedule. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssignmentSchedule.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssignmentSchedule') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of AssignmentSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAssignmentSchedule <- function(AssignmentScheduleID, AssignmentID = F, StartDate = F, EndDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AssignmentScheduleID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "AssignmentSchedule", objectId = AssignmentScheduleID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AssignmentSchedule
	#'
	#' This function deletes an AssignmentSchedule
	#' @param AssignmentScheduleID The ID of the AssignmentSchedule to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The AssignmentScheduleID of the deleted AssignmentSchedule.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAssignmentSchedule <- function(AssignmentScheduleID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "AssignmentSchedule", objectId = AssignmentScheduleID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AssignmentSchedule
	#'
	#' This function creates an AssignmentSchedule
	#' @param fieldNames The field values to give the created AssignmentSchedule. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created AssignmentSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAssignmentSchedule <- function(AssignmentID = NULL, StartDate = NULL, EndDate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "AssignmentSchedule", body = list(DataObject = body), searchFields = append("AssignmentScheduleID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AssignmentSchedule
	#'
	#' This function modifies an AssignmentSchedule
	#' @param fieldNames The field values to give the modified AssignmentSchedule. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified AssignmentSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAssignmentSchedule <- function(AssignmentScheduleID, AssignmentID = NULL, StartDate = NULL, EndDate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "AssignmentSchedule", objectId = AssignmentScheduleID, body = list(DataObject = body), searchFields = append("AssignmentScheduleID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AssignmentScheduleDetails
	#'
	#' This function returns a dataframe or json object of AssignmentScheduleDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssignmentScheduleDetails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssignmentScheduleDetails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssignmentScheduleDetail') to get more field paths.
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
	#' @concept Position
	#' @return A list of AssignmentScheduleDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAssignmentScheduleDetails <- function(searchConditionsList = NULL, AssignmentScheduleDetailID = F, AssignmentScheduleID = F, StartTime = F, EndTime = F, Status = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "AssignmentScheduleDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AssignmentScheduleDetail
	#'
	#' This function returns a dataframe or json object of an AssignmentScheduleDetail
	#' @param AssignmentScheduleDetailID The ID of the AssignmentScheduleDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssignmentScheduleDetail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssignmentScheduleDetail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssignmentScheduleDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of AssignmentScheduleDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAssignmentScheduleDetail <- function(AssignmentScheduleDetailID, AssignmentScheduleID = F, StartTime = F, EndTime = F, Status = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AssignmentScheduleDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "AssignmentScheduleDetail", objectId = AssignmentScheduleDetailID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AssignmentScheduleDetail
	#'
	#' This function deletes an AssignmentScheduleDetail
	#' @param AssignmentScheduleDetailID The ID of the AssignmentScheduleDetail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The AssignmentScheduleDetailID of the deleted AssignmentScheduleDetail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAssignmentScheduleDetail <- function(AssignmentScheduleDetailID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "AssignmentScheduleDetail", objectId = AssignmentScheduleDetailID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AssignmentScheduleDetail
	#'
	#' This function creates an AssignmentScheduleDetail
	#' @param fieldNames The field values to give the created AssignmentScheduleDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created AssignmentScheduleDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAssignmentScheduleDetail <- function(AssignmentScheduleID = NULL, StartTime = NULL, EndTime = NULL, Status = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "AssignmentScheduleDetail", body = list(DataObject = body), searchFields = append("AssignmentScheduleDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AssignmentScheduleDetail
	#'
	#' This function modifies an AssignmentScheduleDetail
	#' @param fieldNames The field values to give the modified AssignmentScheduleDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified AssignmentScheduleDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAssignmentScheduleDetail <- function(AssignmentScheduleDetailID, AssignmentScheduleID = NULL, StartTime = NULL, EndTime = NULL, Status = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "AssignmentScheduleDetail", objectId = AssignmentScheduleDetailID, body = list(DataObject = body), searchFields = append("AssignmentScheduleDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StatePERAPositionClassMNS
	#'
	#' This function returns a dataframe or json object of StatePERAPositionClassMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StatePERAPositionClassMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StatePERAPositionClassMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StatePERAPositionClassMN') to get more field paths.
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
	#' @concept Position
	#' @return A list of StatePERAPositionClassMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStatePERAPositionClassMNS <- function(searchConditionsList = NULL, StatePERAPositionClassMNID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "StatePERAPositionClassMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StatePERAPositionClassMN
	#'
	#' This function returns a dataframe or json object of a StatePERAPositionClassMN
	#' @param StatePERAPositionClassMNID The ID of the StatePERAPositionClassMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StatePERAPositionClassMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StatePERAPositionClassMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StatePERAPositionClassMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of StatePERAPositionClassMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStatePERAPositionClassMN <- function(StatePERAPositionClassMNID, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StatePERAPositionClassMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "StatePERAPositionClassMN", objectId = StatePERAPositionClassMNID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StatePERAPositionClassMN
	#'
	#' This function deletes a StatePERAPositionClassMN
	#' @param StatePERAPositionClassMNID The ID of the StatePERAPositionClassMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The StatePERAPositionClassMNID of the deleted StatePERAPositionClassMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStatePERAPositionClassMN <- function(StatePERAPositionClassMNID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "StatePERAPositionClassMN", objectId = StatePERAPositionClassMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StatePERAPositionClassMN
	#'
	#' This function creates a StatePERAPositionClassMN
	#' @param fieldNames The field values to give the created StatePERAPositionClassMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created StatePERAPositionClassMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStatePERAPositionClassMN <- function(Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "StatePERAPositionClassMN", body = list(DataObject = body), searchFields = append("StatePERAPositionClassMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StatePERAPositionClassMN
	#'
	#' This function modifies a StatePERAPositionClassMN
	#' @param fieldNames The field values to give the modified StatePERAPositionClassMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified StatePERAPositionClassMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStatePERAPositionClassMN <- function(StatePERAPositionClassMNID, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "StatePERAPositionClassMN", objectId = StatePERAPositionClassMNID, body = list(DataObject = body), searchFields = append("StatePERAPositionClassMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StatePERAPositionCodeMNS
	#'
	#' This function returns a dataframe or json object of StatePERAPositionCodeMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StatePERAPositionCodeMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StatePERAPositionCodeMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StatePERAPositionCodeMN') to get more field paths.
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
	#' @concept Position
	#' @return A list of StatePERAPositionCodeMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStatePERAPositionCodeMNS <- function(searchConditionsList = NULL, StatePERAPositionCodeMNID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "StatePERAPositionCodeMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StatePERAPositionCodeMN
	#'
	#' This function returns a dataframe or json object of a StatePERAPositionCodeMN
	#' @param StatePERAPositionCodeMNID The ID of the StatePERAPositionCodeMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StatePERAPositionCodeMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StatePERAPositionCodeMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StatePERAPositionCodeMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of StatePERAPositionCodeMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStatePERAPositionCodeMN <- function(StatePERAPositionCodeMNID, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StatePERAPositionCodeMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "StatePERAPositionCodeMN", objectId = StatePERAPositionCodeMNID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StatePERAPositionCodeMN
	#'
	#' This function deletes a StatePERAPositionCodeMN
	#' @param StatePERAPositionCodeMNID The ID of the StatePERAPositionCodeMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The StatePERAPositionCodeMNID of the deleted StatePERAPositionCodeMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStatePERAPositionCodeMN <- function(StatePERAPositionCodeMNID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "StatePERAPositionCodeMN", objectId = StatePERAPositionCodeMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StatePERAPositionCodeMN
	#'
	#' This function creates a StatePERAPositionCodeMN
	#' @param fieldNames The field values to give the created StatePERAPositionCodeMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created StatePERAPositionCodeMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStatePERAPositionCodeMN <- function(Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "StatePERAPositionCodeMN", body = list(DataObject = body), searchFields = append("StatePERAPositionCodeMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StatePERAPositionCodeMN
	#'
	#' This function modifies a StatePERAPositionCodeMN
	#' @param fieldNames The field values to give the modified StatePERAPositionCodeMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified StatePERAPositionCodeMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStatePERAPositionCodeMN <- function(StatePERAPositionCodeMNID, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "StatePERAPositionCodeMN", objectId = StatePERAPositionCodeMNID, body = list(DataObject = body), searchFields = append("StatePERAPositionCodeMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StateTRAEligibilityMNS
	#'
	#' This function returns a dataframe or json object of StateTRAEligibilityMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StateTRAEligibilityMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StateTRAEligibilityMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StateTRAEligibilityMN') to get more field paths.
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
	#' @concept Position
	#' @return A list of StateTRAEligibilityMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStateTRAEligibilityMNS <- function(searchConditionsList = NULL, StateTRAEligibilityMNID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "StateTRAEligibilityMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StateTRAEligibilityMN
	#'
	#' This function returns a dataframe or json object of a StateTRAEligibilityMN
	#' @param StateTRAEligibilityMNID The ID of the StateTRAEligibilityMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StateTRAEligibilityMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StateTRAEligibilityMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StateTRAEligibilityMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of StateTRAEligibilityMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStateTRAEligibilityMN <- function(StateTRAEligibilityMNID, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StateTRAEligibilityMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "StateTRAEligibilityMN", objectId = StateTRAEligibilityMNID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StateTRAEligibilityMN
	#'
	#' This function deletes a StateTRAEligibilityMN
	#' @param StateTRAEligibilityMNID The ID of the StateTRAEligibilityMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The StateTRAEligibilityMNID of the deleted StateTRAEligibilityMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStateTRAEligibilityMN <- function(StateTRAEligibilityMNID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "StateTRAEligibilityMN", objectId = StateTRAEligibilityMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StateTRAEligibilityMN
	#'
	#' This function creates a StateTRAEligibilityMN
	#' @param fieldNames The field values to give the created StateTRAEligibilityMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created StateTRAEligibilityMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStateTRAEligibilityMN <- function(Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "StateTRAEligibilityMN", body = list(DataObject = body), searchFields = append("StateTRAEligibilityMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StateTRAEligibilityMN
	#'
	#' This function modifies a StateTRAEligibilityMN
	#' @param fieldNames The field values to give the modified StateTRAEligibilityMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified StateTRAEligibilityMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStateTRAEligibilityMN <- function(StateTRAEligibilityMNID, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "StateTRAEligibilityMN", objectId = StateTRAEligibilityMNID, body = list(DataObject = body), searchFields = append("StateTRAEligibilityMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PositionTypes
	#'
	#' This function returns a dataframe or json object of PositionTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PositionTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PositionTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PositionType') to get more field paths.
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
	#' @concept Position
	#' @return A list of PositionTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPositionTypes <- function(searchConditionsList = NULL, PositionTypeMNID = F, StateTRACurrentPositionMNID = F, StateRetirementAssociationTypeMNID = F, StateTRAEligibilityMNID = F, StatePERAExclusionCodeMNIDDefault = F, StatePERAPositionCodeMNIDDefault = F, StatePERAPositionClassMNIDDefault = F, RetirementAssociation = F, PositionTypeID = F, Code = F, Description = F, DistrictID = F, FiscalYearID = F, SalaryCalculationMethodID = F, EntitlementID = F, PayScaleID = F, TaxableLifeInsuranceFactor = F, CodeDescription = F, BudgetedFTE = F, ClosingAssignmentFTE = F, VacantClosingAssignmentFTE = F, PositionTypeIDClonedFrom = F, AssignmentTimeTrackingGroupIDDefault = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, STARPeriodsPerWeek = F, STARLengthOfPeriod = F, StateReportingDistributionSummaryCurrentFTE = F, StateReportingDistributionSummaryClosingAssignmentFTE = F, StateReportingDistributionSummaryVacantClosingAssignmentFTE = F, PlanPositionDistributionsForPlanGroupFTE = F, BenefitGroupID = F, MatrixID = F, CalendarID = F, FullPaySecondsPerDay = F, AllowTimeOff = F, FTEGroupID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "PositionType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PositionType
	#'
	#' This function returns a dataframe or json object of a PositionType
	#' @param PositionTypeID The ID of the PositionType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PositionType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PositionType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PositionType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of PositionType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPositionType <- function(PositionTypeID, PositionTypeMNID = F, StateTRACurrentPositionMNID = F, StateRetirementAssociationTypeMNID = F, StateTRAEligibilityMNID = F, StatePERAExclusionCodeMNIDDefault = F, StatePERAPositionCodeMNIDDefault = F, StatePERAPositionClassMNIDDefault = F, RetirementAssociation = F, Code = F, Description = F, DistrictID = F, FiscalYearID = F, SalaryCalculationMethodID = F, EntitlementID = F, PayScaleID = F, TaxableLifeInsuranceFactor = F, CodeDescription = F, BudgetedFTE = F, ClosingAssignmentFTE = F, VacantClosingAssignmentFTE = F, PositionTypeIDClonedFrom = F, AssignmentTimeTrackingGroupIDDefault = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, STARPeriodsPerWeek = F, STARLengthOfPeriod = F, StateReportingDistributionSummaryCurrentFTE = F, StateReportingDistributionSummaryClosingAssignmentFTE = F, StateReportingDistributionSummaryVacantClosingAssignmentFTE = F, PlanPositionDistributionsForPlanGroupFTE = F, BenefitGroupID = F, MatrixID = F, CalendarID = F, FullPaySecondsPerDay = F, AllowTimeOff = F, FTEGroupID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PositionTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "PositionType", objectId = PositionTypeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PositionType
	#'
	#' This function deletes a PositionType
	#' @param PositionTypeID The ID of the PositionType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The PositionTypeID of the deleted PositionType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePositionType <- function(PositionTypeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "PositionType", objectId = PositionTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PositionType
	#'
	#' This function creates a PositionType
	#' @param fieldNames The field values to give the created PositionType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created PositionType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPositionType <- function(StateTRACurrentPositionMNID = NULL, StateRetirementAssociationTypeMNID = NULL, StateTRAEligibilityMNID = NULL, StatePERAExclusionCodeMNIDDefault = NULL, StatePERAPositionCodeMNIDDefault = NULL, StatePERAPositionClassMNIDDefault = NULL, RetirementAssociation = NULL, Code = NULL, Description = NULL, DistrictID = NULL, FiscalYearID = NULL, SalaryCalculationMethodID = NULL, EntitlementID = NULL, PayScaleID = NULL, TaxableLifeInsuranceFactor = NULL, PositionTypeIDClonedFrom = NULL, AssignmentTimeTrackingGroupIDDefault = NULL, BenefitGroupID = NULL, MatrixID = NULL, CalendarID = NULL, FullPaySecondsPerDay = NULL, AllowTimeOff = NULL, FTEGroupID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "PositionType", body = list(DataObject = body), searchFields = append("PositionTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PositionType
	#'
	#' This function modifies a PositionType
	#' @param fieldNames The field values to give the modified PositionType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified PositionType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPositionType <- function(PositionTypeID, StateTRACurrentPositionMNID = NULL, StateRetirementAssociationTypeMNID = NULL, StateTRAEligibilityMNID = NULL, StatePERAExclusionCodeMNIDDefault = NULL, StatePERAPositionCodeMNIDDefault = NULL, StatePERAPositionClassMNIDDefault = NULL, RetirementAssociation = NULL, Code = NULL, Description = NULL, DistrictID = NULL, FiscalYearID = NULL, SalaryCalculationMethodID = NULL, EntitlementID = NULL, PayScaleID = NULL, TaxableLifeInsuranceFactor = NULL, PositionTypeIDClonedFrom = NULL, AssignmentTimeTrackingGroupIDDefault = NULL, BenefitGroupID = NULL, MatrixID = NULL, CalendarID = NULL, FullPaySecondsPerDay = NULL, AllowTimeOff = NULL, FTEGroupID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "PositionType", objectId = PositionTypeID, body = list(DataObject = body), searchFields = append("PositionTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StateTRACurrentPositionMNS
	#'
	#' This function returns a dataframe or json object of StateTRACurrentPositionMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StateTRACurrentPositionMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StateTRACurrentPositionMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StateTRACurrentPositionMN') to get more field paths.
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
	#' @concept Position
	#' @return A list of StateTRACurrentPositionMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStateTRACurrentPositionMNS <- function(searchConditionsList = NULL, StateTRACurrentPositionMNID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "StateTRACurrentPositionMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StateTRACurrentPositionMN
	#'
	#' This function returns a dataframe or json object of a StateTRACurrentPositionMN
	#' @param StateTRACurrentPositionMNID The ID of the StateTRACurrentPositionMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StateTRACurrentPositionMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StateTRACurrentPositionMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StateTRACurrentPositionMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of StateTRACurrentPositionMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStateTRACurrentPositionMN <- function(StateTRACurrentPositionMNID, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StateTRACurrentPositionMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "StateTRACurrentPositionMN", objectId = StateTRACurrentPositionMNID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StateTRACurrentPositionMN
	#'
	#' This function deletes a StateTRACurrentPositionMN
	#' @param StateTRACurrentPositionMNID The ID of the StateTRACurrentPositionMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The StateTRACurrentPositionMNID of the deleted StateTRACurrentPositionMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStateTRACurrentPositionMN <- function(StateTRACurrentPositionMNID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "StateTRACurrentPositionMN", objectId = StateTRACurrentPositionMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StateTRACurrentPositionMN
	#'
	#' This function creates a StateTRACurrentPositionMN
	#' @param fieldNames The field values to give the created StateTRACurrentPositionMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created StateTRACurrentPositionMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStateTRACurrentPositionMN <- function(Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "StateTRACurrentPositionMN", body = list(DataObject = body), searchFields = append("StateTRACurrentPositionMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StateTRACurrentPositionMN
	#'
	#' This function modifies a StateTRACurrentPositionMN
	#' @param fieldNames The field values to give the modified StateTRACurrentPositionMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified StateTRACurrentPositionMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStateTRACurrentPositionMN <- function(StateTRACurrentPositionMNID, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "StateTRACurrentPositionMN", objectId = StateTRACurrentPositionMNID, body = list(DataObject = body), searchFields = append("StateTRACurrentPositionMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PositionTempAssignmentErrors
	#'
	#' This function returns a dataframe or json object of PositionTempAssignmentErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PositionTempAssignmentErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PositionTempAssignmentErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PositionTempAssignmentError') to get more field paths.
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
	#' @concept Position
	#' @return A list of PositionTempAssignmentErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPositionTempAssignmentErrors <- function(searchConditionsList = NULL, TempAssignmentErrorID = F, PositionNumberCode = F, Employee = F, ErrorReason = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalPay = F, EmployeeNumber = F, PositionTypeCode = F, PositionTypeDescription = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, TempAssignmentID = F, ErrorField = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempAssignmentError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PositionTempAssignmentError
	#'
	#' This function returns a dataframe or json object of a PositionTempAssignmentError
	#' @param PositionTempAssignmentErrorID The ID of the PositionTempAssignmentError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PositionTempAssignmentError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PositionTempAssignmentError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PositionTempAssignmentError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of PositionTempAssignmentError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPositionTempAssignmentError <- function(PositionTempAssignmentErrorID, TempAssignmentErrorID = F, PositionNumberCode = F, Employee = F, ErrorReason = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalPay = F, EmployeeNumber = F, PositionTypeCode = F, PositionTypeDescription = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, TempAssignmentID = F, ErrorField = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PositionTempAssignmentErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "TempAssignmentError", objectId = PositionTempAssignmentErrorID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PositionTempAssignmentError
	#'
	#' This function deletes a PositionTempAssignmentError
	#' @param PositionTempAssignmentErrorID The ID of the PositionTempAssignmentError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The PositionTempAssignmentErrorID of the deleted PositionTempAssignmentError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePositionTempAssignmentError <- function(PositionTempAssignmentErrorID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "TempAssignmentError", objectId = PositionTempAssignmentErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PositionTempAssignmentError
	#'
	#' This function creates a PositionTempAssignmentError
	#' @param fieldNames The field values to give the created PositionTempAssignmentError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created PositionTempAssignmentError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPositionTempAssignmentError <- function(PositionNumberCode = NULL, Employee = NULL, ErrorReason = NULL, TotalPay = NULL, EmployeeNumber = NULL, PositionTypeCode = NULL, PositionTypeDescription = NULL, PositionDistributionsAssignmentTypeCodes = NULL, PositionDistributionsAssignmentTypeDescriptions = NULL, PositionDistributionsBuildingCodes = NULL, PositionDistributionsBuildingDescriptions = NULL, TempAssignmentID = NULL, ErrorField = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "TempAssignmentError", body = list(DataObject = body), searchFields = append("TempAssignmentErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PositionTempAssignmentError
	#'
	#' This function modifies a PositionTempAssignmentError
	#' @param fieldNames The field values to give the modified PositionTempAssignmentError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified PositionTempAssignmentError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPositionTempAssignmentError <- function(TempAssignmentErrorID, PositionNumberCode = NULL, Employee = NULL, ErrorReason = NULL, TotalPay = NULL, EmployeeNumber = NULL, PositionTypeCode = NULL, PositionTypeDescription = NULL, PositionDistributionsAssignmentTypeCodes = NULL, PositionDistributionsAssignmentTypeDescriptions = NULL, PositionDistributionsBuildingCodes = NULL, PositionDistributionsBuildingDescriptions = NULL, TempAssignmentID = NULL, ErrorField = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "TempAssignmentError", objectId = TempAssignmentErrorID, body = list(DataObject = body), searchFields = append("TempAssignmentErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempAssignments
	#'
	#' This function returns a dataframe or json object of TempAssignments
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAssignments. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAssignments.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAssignment') to get more field paths.
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
	#' @concept Position
	#' @return A list of TempAssignments
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempAssignments <- function(searchConditionsList = NULL, TempAssignmentID = F, PositionNumberCode = F, EmployeeID = F, Employee = F, PositionID = F, SalaryCalculationMethodID = F, NextYearIntentID = F, NextYearIntentCodeDescription = F, MatrixID = F, MatrixIDBase = F, AssignmentDetailStartDate = F, AssignmentDetailEndDate = F, AssignmentDetailIsPrimary = F, AssignmentDetailTotalPay = F, AssignmentTimeTrackingGroupID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AssignmentID = F, AssignmentStartDate = F, AssignmentEndDate = F, TotalPay = F, NewTotalPay = F, DailyPay = F, NewDailyPay = F, HourlyPay = F, NewHourlyPay = F, AnnualizedPay = F, NewAnnualizedPay = F, EmployeeNumber = F, OldEntitlementCodeDescription = F, NewEntitlementCodeDescription = F, OldNextYearIntentCodeDescription = F, NewNextYearIntentCodeDescription = F, OldAssignmentTimeTrackingGroupCodeDescription = F, NewAssignmentTimeTrackingGroupCodeDescription = F, OldRetirementJobCategoryCodeDescription = F, RetirementJobCategoryCodeDescription = F, OldRetirementWorkStatusCodeDescription = F, RetirementWorkStatusCodeDescription = F, OldChapter40TermDate = F, Chapter40TermDate = F, PositionTypeCode = F, PositionTypeDescription = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, AssignmentDetailFormattedSecondsPerDay = F, EntitlementCode = F, AssignmentDetailIsOverloaded = F, PositionBudgetedFTE = F, ErrorCount = F, TempPositionID = F, EmployeePlacementID = F, EntitlementID = F, IsTotalPayChanging = F, ActivePaidDayCount = F, NewActivePaidDayCount = F, WorkdayCount = F, NewWorkdayCount = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempAssignment", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempAssignment
	#'
	#' This function returns a dataframe or json object of a TempAssignment
	#' @param TempAssignmentID The ID of the TempAssignment to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAssignment. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAssignment.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAssignment') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of TempAssignment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempAssignment <- function(TempAssignmentID, PositionNumberCode = F, EmployeeID = F, Employee = F, PositionID = F, SalaryCalculationMethodID = F, NextYearIntentID = F, NextYearIntentCodeDescription = F, MatrixID = F, MatrixIDBase = F, AssignmentDetailStartDate = F, AssignmentDetailEndDate = F, AssignmentDetailIsPrimary = F, AssignmentDetailTotalPay = F, AssignmentTimeTrackingGroupID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AssignmentID = F, AssignmentStartDate = F, AssignmentEndDate = F, TotalPay = F, NewTotalPay = F, DailyPay = F, NewDailyPay = F, HourlyPay = F, NewHourlyPay = F, AnnualizedPay = F, NewAnnualizedPay = F, EmployeeNumber = F, OldEntitlementCodeDescription = F, NewEntitlementCodeDescription = F, OldNextYearIntentCodeDescription = F, NewNextYearIntentCodeDescription = F, OldAssignmentTimeTrackingGroupCodeDescription = F, NewAssignmentTimeTrackingGroupCodeDescription = F, OldRetirementJobCategoryCodeDescription = F, RetirementJobCategoryCodeDescription = F, OldRetirementWorkStatusCodeDescription = F, RetirementWorkStatusCodeDescription = F, OldChapter40TermDate = F, Chapter40TermDate = F, PositionTypeCode = F, PositionTypeDescription = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, AssignmentDetailFormattedSecondsPerDay = F, EntitlementCode = F, AssignmentDetailIsOverloaded = F, PositionBudgetedFTE = F, ErrorCount = F, TempPositionID = F, EmployeePlacementID = F, EntitlementID = F, IsTotalPayChanging = F, ActivePaidDayCount = F, NewActivePaidDayCount = F, WorkdayCount = F, NewWorkdayCount = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempAssignmentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "TempAssignment", objectId = TempAssignmentID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempAssignment
	#'
	#' This function deletes a TempAssignment
	#' @param TempAssignmentID The ID of the TempAssignment to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The TempAssignmentID of the deleted TempAssignment.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempAssignment <- function(TempAssignmentID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "TempAssignment", objectId = TempAssignmentID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempAssignment
	#'
	#' This function creates a TempAssignment
	#' @param fieldNames The field values to give the created TempAssignment. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created TempAssignment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempAssignment <- function(PositionNumberCode = NULL, EmployeeID = NULL, Employee = NULL, PositionID = NULL, SalaryCalculationMethodID = NULL, NextYearIntentID = NULL, NextYearIntentCodeDescription = NULL, MatrixID = NULL, MatrixIDBase = NULL, AssignmentDetailStartDate = NULL, AssignmentDetailEndDate = NULL, AssignmentDetailIsPrimary = NULL, AssignmentDetailTotalPay = NULL, AssignmentTimeTrackingGroupID = NULL, AssignmentID = NULL, AssignmentStartDate = NULL, AssignmentEndDate = NULL, TotalPay = NULL, NewTotalPay = NULL, DailyPay = NULL, NewDailyPay = NULL, HourlyPay = NULL, NewHourlyPay = NULL, AnnualizedPay = NULL, NewAnnualizedPay = NULL, EmployeeNumber = NULL, OldEntitlementCodeDescription = NULL, NewEntitlementCodeDescription = NULL, OldNextYearIntentCodeDescription = NULL, NewNextYearIntentCodeDescription = NULL, OldAssignmentTimeTrackingGroupCodeDescription = NULL, NewAssignmentTimeTrackingGroupCodeDescription = NULL, OldRetirementJobCategoryCodeDescription = NULL, RetirementJobCategoryCodeDescription = NULL, OldRetirementWorkStatusCodeDescription = NULL, RetirementWorkStatusCodeDescription = NULL, OldChapter40TermDate = NULL, Chapter40TermDate = NULL, PositionTypeCode = NULL, PositionTypeDescription = NULL, PositionDistributionsAssignmentTypeCodes = NULL, PositionDistributionsAssignmentTypeDescriptions = NULL, PositionDistributionsBuildingCodes = NULL, PositionDistributionsBuildingDescriptions = NULL, AssignmentDetailFormattedSecondsPerDay = NULL, EntitlementCode = NULL, AssignmentDetailIsOverloaded = NULL, PositionBudgetedFTE = NULL, ErrorCount = NULL, TempPositionID = NULL, EmployeePlacementID = NULL, EntitlementID = NULL, IsTotalPayChanging = NULL, ActivePaidDayCount = NULL, NewActivePaidDayCount = NULL, WorkdayCount = NULL, NewWorkdayCount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "TempAssignment", body = list(DataObject = body), searchFields = append("TempAssignmentID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempAssignment
	#'
	#' This function modifies a TempAssignment
	#' @param fieldNames The field values to give the modified TempAssignment. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified TempAssignment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempAssignment <- function(TempAssignmentID, PositionNumberCode = NULL, EmployeeID = NULL, Employee = NULL, PositionID = NULL, SalaryCalculationMethodID = NULL, NextYearIntentID = NULL, NextYearIntentCodeDescription = NULL, MatrixID = NULL, MatrixIDBase = NULL, AssignmentDetailStartDate = NULL, AssignmentDetailEndDate = NULL, AssignmentDetailIsPrimary = NULL, AssignmentDetailTotalPay = NULL, AssignmentTimeTrackingGroupID = NULL, AssignmentID = NULL, AssignmentStartDate = NULL, AssignmentEndDate = NULL, TotalPay = NULL, NewTotalPay = NULL, DailyPay = NULL, NewDailyPay = NULL, HourlyPay = NULL, NewHourlyPay = NULL, AnnualizedPay = NULL, NewAnnualizedPay = NULL, EmployeeNumber = NULL, OldEntitlementCodeDescription = NULL, NewEntitlementCodeDescription = NULL, OldNextYearIntentCodeDescription = NULL, NewNextYearIntentCodeDescription = NULL, OldAssignmentTimeTrackingGroupCodeDescription = NULL, NewAssignmentTimeTrackingGroupCodeDescription = NULL, OldRetirementJobCategoryCodeDescription = NULL, RetirementJobCategoryCodeDescription = NULL, OldRetirementWorkStatusCodeDescription = NULL, RetirementWorkStatusCodeDescription = NULL, OldChapter40TermDate = NULL, Chapter40TermDate = NULL, PositionTypeCode = NULL, PositionTypeDescription = NULL, PositionDistributionsAssignmentTypeCodes = NULL, PositionDistributionsAssignmentTypeDescriptions = NULL, PositionDistributionsBuildingCodes = NULL, PositionDistributionsBuildingDescriptions = NULL, AssignmentDetailFormattedSecondsPerDay = NULL, EntitlementCode = NULL, AssignmentDetailIsOverloaded = NULL, PositionBudgetedFTE = NULL, ErrorCount = NULL, TempPositionID = NULL, EmployeePlacementID = NULL, EntitlementID = NULL, IsTotalPayChanging = NULL, ActivePaidDayCount = NULL, NewActivePaidDayCount = NULL, WorkdayCount = NULL, NewWorkdayCount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "TempAssignment", objectId = TempAssignmentID, body = list(DataObject = body), searchFields = append("TempAssignmentID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempPositions
	#'
	#' This function returns a dataframe or json object of TempPositions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempPositions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempPositions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempPosition') to get more field paths.
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
	#' @concept Position
	#' @return A list of TempPositions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempPositions <- function(searchConditionsList = NULL, TempPositionID = F, PositionIDClonedFrom = F, PositionNumberCode = F, PositionNumberID = F, PositionTypeCodeDescription = F, PositionTypeID = F, CalendarID = F, CalendarCodeDescription = F, StartDate = F, EndDate = F, FullPaySecondsPerDay = F, BudgetedFTE = F, PositionGroupID = F, JobTypeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ConfigFiscalYearTRSStateBaseLaneID = F, PlanPositionIDClonedFrom = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, PositionDistributionsFTEGroupCodes = F, PositionDistributionsFTEGroupDescriptions = F, ActivePaidDayCount = F, TotalPaidSeconds = F, FullPaidDays = F, PlanPositionAnnualPay = F, Employee = F, EmployeeNumber = F, AnnualPay = F, IsPrimary = F, ErrorCount = F, PositionID = F, EmployeeID = F, DailyPay = F, HourlyPay = F, CanBeDeleted = F, SalaryCalculationMethodID = F, EntitlementID = F, EmployeePlacementID = F, AssignmentTimeTrackingGroupID = F, NextYearIntentID = F, AssignmentStartDate = F, AssignmentEndDate = F, AssignmentSecondsPerDay = F, AssignmentEnteredFTE = F, AssignmentEnteredRate = F, MatrixID = F, EmployeePlacementDetailID = F, TempEmployeePlacementDetailID = F, StepID = F, StepValue = F, Exception = F, LineNumber = F, WillCreatePosition = F, WillCreatePositionNumber = F, EmployeeDistrictID = F, ConfigFiscalYearTRSStateBaseStepID = F, PositionTypeCode = F, OldActivePaidDayCount = F, FormattedTotalPaidSeconds = F, OldTotalPaidSeconds = F, OldFormattedTotalPaidSeconds = F, OldFullPaidDays = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempPosition", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempPosition
	#'
	#' This function returns a dataframe or json object of a TempPosition
	#' @param TempPositionID The ID of the TempPosition to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempPosition. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempPosition.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempPosition') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of TempPosition
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempPosition <- function(TempPositionID, PositionIDClonedFrom = F, PositionNumberCode = F, PositionNumberID = F, PositionTypeCodeDescription = F, PositionTypeID = F, CalendarID = F, CalendarCodeDescription = F, StartDate = F, EndDate = F, FullPaySecondsPerDay = F, BudgetedFTE = F, PositionGroupID = F, JobTypeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ConfigFiscalYearTRSStateBaseLaneID = F, PlanPositionIDClonedFrom = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, PositionDistributionsFTEGroupCodes = F, PositionDistributionsFTEGroupDescriptions = F, ActivePaidDayCount = F, TotalPaidSeconds = F, FullPaidDays = F, PlanPositionAnnualPay = F, Employee = F, EmployeeNumber = F, AnnualPay = F, IsPrimary = F, ErrorCount = F, PositionID = F, EmployeeID = F, DailyPay = F, HourlyPay = F, CanBeDeleted = F, SalaryCalculationMethodID = F, EntitlementID = F, EmployeePlacementID = F, AssignmentTimeTrackingGroupID = F, NextYearIntentID = F, AssignmentStartDate = F, AssignmentEndDate = F, AssignmentSecondsPerDay = F, AssignmentEnteredFTE = F, AssignmentEnteredRate = F, MatrixID = F, EmployeePlacementDetailID = F, TempEmployeePlacementDetailID = F, StepID = F, StepValue = F, Exception = F, LineNumber = F, WillCreatePosition = F, WillCreatePositionNumber = F, EmployeeDistrictID = F, ConfigFiscalYearTRSStateBaseStepID = F, PositionTypeCode = F, OldActivePaidDayCount = F, FormattedTotalPaidSeconds = F, OldTotalPaidSeconds = F, OldFormattedTotalPaidSeconds = F, OldFullPaidDays = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempPositionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "TempPosition", objectId = TempPositionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempPosition
	#'
	#' This function deletes a TempPosition
	#' @param TempPositionID The ID of the TempPosition to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The TempPositionID of the deleted TempPosition.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempPosition <- function(TempPositionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "TempPosition", objectId = TempPositionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempPosition
	#'
	#' This function creates a TempPosition
	#' @param fieldNames The field values to give the created TempPosition. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created TempPosition
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempPosition <- function(PositionIDClonedFrom = NULL, PositionNumberCode = NULL, PositionNumberID = NULL, PositionTypeCodeDescription = NULL, PositionTypeID = NULL, CalendarID = NULL, CalendarCodeDescription = NULL, StartDate = NULL, EndDate = NULL, FullPaySecondsPerDay = NULL, BudgetedFTE = NULL, PositionGroupID = NULL, JobTypeID = NULL, ConfigFiscalYearTRSStateBaseLaneID = NULL, PlanPositionIDClonedFrom = NULL, PositionDistributionsAssignmentTypeCodes = NULL, PositionDistributionsAssignmentTypeDescriptions = NULL, PositionDistributionsBuildingCodes = NULL, PositionDistributionsBuildingDescriptions = NULL, PositionDistributionsFTEGroupCodes = NULL, PositionDistributionsFTEGroupDescriptions = NULL, ActivePaidDayCount = NULL, TotalPaidSeconds = NULL, FullPaidDays = NULL, PlanPositionAnnualPay = NULL, Employee = NULL, EmployeeNumber = NULL, AnnualPay = NULL, IsPrimary = NULL, ErrorCount = NULL, PositionID = NULL, EmployeeID = NULL, DailyPay = NULL, HourlyPay = NULL, CanBeDeleted = NULL, SalaryCalculationMethodID = NULL, EntitlementID = NULL, EmployeePlacementID = NULL, AssignmentTimeTrackingGroupID = NULL, NextYearIntentID = NULL, AssignmentStartDate = NULL, AssignmentEndDate = NULL, AssignmentSecondsPerDay = NULL, AssignmentEnteredFTE = NULL, AssignmentEnteredRate = NULL, MatrixID = NULL, EmployeePlacementDetailID = NULL, TempEmployeePlacementDetailID = NULL, StepID = NULL, StepValue = NULL, Exception = NULL, LineNumber = NULL, WillCreatePosition = NULL, WillCreatePositionNumber = NULL, EmployeeDistrictID = NULL, ConfigFiscalYearTRSStateBaseStepID = NULL, PositionTypeCode = NULL, OldActivePaidDayCount = NULL, OldTotalPaidSeconds = NULL, OldFullPaidDays = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "TempPosition", body = list(DataObject = body), searchFields = append("TempPositionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempPosition
	#'
	#' This function modifies a TempPosition
	#' @param fieldNames The field values to give the modified TempPosition. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified TempPosition
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempPosition <- function(TempPositionID, PositionIDClonedFrom = NULL, PositionNumberCode = NULL, PositionNumberID = NULL, PositionTypeCodeDescription = NULL, PositionTypeID = NULL, CalendarID = NULL, CalendarCodeDescription = NULL, StartDate = NULL, EndDate = NULL, FullPaySecondsPerDay = NULL, BudgetedFTE = NULL, PositionGroupID = NULL, JobTypeID = NULL, ConfigFiscalYearTRSStateBaseLaneID = NULL, PlanPositionIDClonedFrom = NULL, PositionDistributionsAssignmentTypeCodes = NULL, PositionDistributionsAssignmentTypeDescriptions = NULL, PositionDistributionsBuildingCodes = NULL, PositionDistributionsBuildingDescriptions = NULL, PositionDistributionsFTEGroupCodes = NULL, PositionDistributionsFTEGroupDescriptions = NULL, ActivePaidDayCount = NULL, TotalPaidSeconds = NULL, FullPaidDays = NULL, PlanPositionAnnualPay = NULL, Employee = NULL, EmployeeNumber = NULL, AnnualPay = NULL, IsPrimary = NULL, ErrorCount = NULL, PositionID = NULL, EmployeeID = NULL, DailyPay = NULL, HourlyPay = NULL, CanBeDeleted = NULL, SalaryCalculationMethodID = NULL, EntitlementID = NULL, EmployeePlacementID = NULL, AssignmentTimeTrackingGroupID = NULL, NextYearIntentID = NULL, AssignmentStartDate = NULL, AssignmentEndDate = NULL, AssignmentSecondsPerDay = NULL, AssignmentEnteredFTE = NULL, AssignmentEnteredRate = NULL, MatrixID = NULL, EmployeePlacementDetailID = NULL, TempEmployeePlacementDetailID = NULL, StepID = NULL, StepValue = NULL, Exception = NULL, LineNumber = NULL, WillCreatePosition = NULL, WillCreatePositionNumber = NULL, EmployeeDistrictID = NULL, ConfigFiscalYearTRSStateBaseStepID = NULL, PositionTypeCode = NULL, OldActivePaidDayCount = NULL, OldTotalPaidSeconds = NULL, OldFullPaidDays = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "TempPosition", objectId = TempPositionID, body = list(DataObject = body), searchFields = append("TempPositionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempPositionErrors
	#'
	#' This function returns a dataframe or json object of TempPositionErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempPositionErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempPositionErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempPositionError') to get more field paths.
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
	#' @concept Position
	#' @return A list of TempPositionErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempPositionErrors <- function(searchConditionsList = NULL, TempPositionErrorID = F, PositionNumberCode = F, ErrorDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PositionTypeCodeDescription = F, EmployeeFullNameLFM = F, TempPositionID = F, EmployeeNumber = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, PositionDistributionsFTEGroupCodes = F, PositionDistributionsFTEGroupDescriptions = F, FatalException = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempPositionError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempPositionError
	#'
	#' This function returns a dataframe or json object of a TempPositionError
	#' @param TempPositionErrorID The ID of the TempPositionError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempPositionError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempPositionError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempPositionError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of TempPositionError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempPositionError <- function(TempPositionErrorID, PositionNumberCode = F, ErrorDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PositionTypeCodeDescription = F, EmployeeFullNameLFM = F, TempPositionID = F, EmployeeNumber = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, PositionDistributionsFTEGroupCodes = F, PositionDistributionsFTEGroupDescriptions = F, FatalException = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempPositionErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "TempPositionError", objectId = TempPositionErrorID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempPositionError
	#'
	#' This function deletes a TempPositionError
	#' @param TempPositionErrorID The ID of the TempPositionError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The TempPositionErrorID of the deleted TempPositionError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempPositionError <- function(TempPositionErrorID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "TempPositionError", objectId = TempPositionErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempPositionError
	#'
	#' This function creates a TempPositionError
	#' @param fieldNames The field values to give the created TempPositionError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created TempPositionError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempPositionError <- function(PositionNumberCode = NULL, ErrorDescription = NULL, PositionTypeCodeDescription = NULL, EmployeeFullNameLFM = NULL, TempPositionID = NULL, EmployeeNumber = NULL, PositionDistributionsAssignmentTypeCodes = NULL, PositionDistributionsAssignmentTypeDescriptions = NULL, PositionDistributionsBuildingCodes = NULL, PositionDistributionsBuildingDescriptions = NULL, PositionDistributionsFTEGroupCodes = NULL, PositionDistributionsFTEGroupDescriptions = NULL, FatalException = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "TempPositionError", body = list(DataObject = body), searchFields = append("TempPositionErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempPositionError
	#'
	#' This function modifies a TempPositionError
	#' @param fieldNames The field values to give the modified TempPositionError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified TempPositionError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempPositionError <- function(TempPositionErrorID, PositionNumberCode = NULL, ErrorDescription = NULL, PositionTypeCodeDescription = NULL, EmployeeFullNameLFM = NULL, TempPositionID = NULL, EmployeeNumber = NULL, PositionDistributionsAssignmentTypeCodes = NULL, PositionDistributionsAssignmentTypeDescriptions = NULL, PositionDistributionsBuildingCodes = NULL, PositionDistributionsBuildingDescriptions = NULL, PositionDistributionsFTEGroupCodes = NULL, PositionDistributionsFTEGroupDescriptions = NULL, FatalException = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "TempPositionError", objectId = TempPositionErrorID, body = list(DataObject = body), searchFields = append("TempPositionErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List NextYearIntents
	#'
	#' This function returns a dataframe or json object of NextYearIntents
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NextYearIntents. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NextYearIntents.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NextYearIntent') to get more field paths.
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
	#' @concept Position
	#' @return A list of NextYearIntents
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNextYearIntents <- function(searchConditionsList = NULL, NextYearIntentID = F, Code = F, Description = F, DistrictID = F, FiscalYearID = F, CodeDescription = F, NextYearIntentIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "NextYearIntent", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NextYearIntent
	#'
	#' This function returns a dataframe or json object of a NextYearIntent
	#' @param NextYearIntentID The ID of the NextYearIntent to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NextYearIntent. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NextYearIntent.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NextYearIntent') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of NextYearIntent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNextYearIntent <- function(NextYearIntentID, Code = F, Description = F, DistrictID = F, FiscalYearID = F, CodeDescription = F, NextYearIntentIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NextYearIntentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "NextYearIntent", objectId = NextYearIntentID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NextYearIntent
	#'
	#' This function deletes a NextYearIntent
	#' @param NextYearIntentID The ID of the NextYearIntent to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The NextYearIntentID of the deleted NextYearIntent.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNextYearIntent <- function(NextYearIntentID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "NextYearIntent", objectId = NextYearIntentID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NextYearIntent
	#'
	#' This function creates a NextYearIntent
	#' @param fieldNames The field values to give the created NextYearIntent. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created NextYearIntent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNextYearIntent <- function(Code = NULL, Description = NULL, DistrictID = NULL, FiscalYearID = NULL, NextYearIntentIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "NextYearIntent", body = list(DataObject = body), searchFields = append("NextYearIntentID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NextYearIntent
	#'
	#' This function modifies a NextYearIntent
	#' @param fieldNames The field values to give the modified NextYearIntent. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified NextYearIntent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNextYearIntent <- function(NextYearIntentID, Code = NULL, Description = NULL, DistrictID = NULL, FiscalYearID = NULL, NextYearIntentIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "NextYearIntent", objectId = NextYearIntentID, body = list(DataObject = body), searchFields = append("NextYearIntentID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List OrganizationCharts
	#'
	#' This function returns a dataframe or json object of OrganizationCharts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OrganizationCharts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OrganizationCharts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OrganizationChart') to get more field paths.
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
	#' @concept Position
	#' @return A list of OrganizationCharts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listOrganizationCharts <- function(searchConditionsList = NULL, OrganizationChartID = F, DistrictID = F, FiscalYearID = F, Code = F, Description = F, OrganizationChartIDClonedFrom = F, CodeDescription = F, IsUsedInTimeOff = F, IsUsedInTimeTracking = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsUsedInAccountsPayable = F, IsCloneable = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "OrganizationChart", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an OrganizationChart
	#'
	#' This function returns a dataframe or json object of an OrganizationChart
	#' @param OrganizationChartID The ID of the OrganizationChart to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OrganizationChart. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OrganizationChart.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OrganizationChart') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of OrganizationChart
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getOrganizationChart <- function(OrganizationChartID, DistrictID = F, FiscalYearID = F, Code = F, Description = F, OrganizationChartIDClonedFrom = F, CodeDescription = F, IsUsedInTimeOff = F, IsUsedInTimeTracking = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsUsedInAccountsPayable = F, IsCloneable = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "OrganizationChartID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "OrganizationChart", objectId = OrganizationChartID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an OrganizationChart
	#'
	#' This function deletes an OrganizationChart
	#' @param OrganizationChartID The ID of the OrganizationChart to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The OrganizationChartID of the deleted OrganizationChart.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteOrganizationChart <- function(OrganizationChartID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "OrganizationChart", objectId = OrganizationChartID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an OrganizationChart
	#'
	#' This function creates an OrganizationChart
	#' @param fieldNames The field values to give the created OrganizationChart. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created OrganizationChart
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createOrganizationChart <- function(DistrictID = NULL, FiscalYearID = NULL, Code = NULL, Description = NULL, OrganizationChartIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "OrganizationChart", body = list(DataObject = body), searchFields = append("OrganizationChartID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an OrganizationChart
	#'
	#' This function modifies an OrganizationChart
	#' @param fieldNames The field values to give the modified OrganizationChart. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified OrganizationChart
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyOrganizationChart <- function(OrganizationChartID, DistrictID = NULL, FiscalYearID = NULL, Code = NULL, Description = NULL, OrganizationChartIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "OrganizationChart", objectId = OrganizationChartID, body = list(DataObject = body), searchFields = append("OrganizationChartID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List OrganizationChartRelationships
	#'
	#' This function returns a dataframe or json object of OrganizationChartRelationships
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OrganizationChartRelationships. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OrganizationChartRelationships.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OrganizationChartRelationship') to get more field paths.
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
	#' @concept Position
	#' @return A list of OrganizationChartRelationships
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listOrganizationChartRelationships <- function(searchConditionsList = NULL, OrganizationChartRelationshipID = F, OrganizationChartID = F, PositionID = F, PositionIDSupervisor = F, FinalApproval = F, SelfApproval = F, RelationshipIdentifier = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, UsePositionDistributionFilter = F, FilterData = F, StandardFilterCollectionData = F, StandardFilterCollectionSearchCondition = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "OrganizationChartRelationship", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an OrganizationChartRelationship
	#'
	#' This function returns a dataframe or json object of an OrganizationChartRelationship
	#' @param OrganizationChartRelationshipID The ID of the OrganizationChartRelationship to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OrganizationChartRelationship. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OrganizationChartRelationship.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OrganizationChartRelationship') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of OrganizationChartRelationship
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getOrganizationChartRelationship <- function(OrganizationChartRelationshipID, OrganizationChartID = F, PositionID = F, PositionIDSupervisor = F, FinalApproval = F, SelfApproval = F, RelationshipIdentifier = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, UsePositionDistributionFilter = F, FilterData = F, StandardFilterCollectionData = F, StandardFilterCollectionSearchCondition = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "OrganizationChartRelationshipID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "OrganizationChartRelationship", objectId = OrganizationChartRelationshipID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an OrganizationChartRelationship
	#'
	#' This function deletes an OrganizationChartRelationship
	#' @param OrganizationChartRelationshipID The ID of the OrganizationChartRelationship to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The OrganizationChartRelationshipID of the deleted OrganizationChartRelationship.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteOrganizationChartRelationship <- function(OrganizationChartRelationshipID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "OrganizationChartRelationship", objectId = OrganizationChartRelationshipID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an OrganizationChartRelationship
	#'
	#' This function creates an OrganizationChartRelationship
	#' @param fieldNames The field values to give the created OrganizationChartRelationship. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created OrganizationChartRelationship
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createOrganizationChartRelationship <- function(OrganizationChartID = NULL, PositionID = NULL, PositionIDSupervisor = NULL, FinalApproval = NULL, SelfApproval = NULL, UsePositionDistributionFilter = NULL, FilterData = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "OrganizationChartRelationship", body = list(DataObject = body), searchFields = append("OrganizationChartRelationshipID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an OrganizationChartRelationship
	#'
	#' This function modifies an OrganizationChartRelationship
	#' @param fieldNames The field values to give the modified OrganizationChartRelationship. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified OrganizationChartRelationship
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyOrganizationChartRelationship <- function(OrganizationChartRelationshipID, OrganizationChartID = NULL, PositionID = NULL, PositionIDSupervisor = NULL, FinalApproval = NULL, SelfApproval = NULL, UsePositionDistributionFilter = NULL, FilterData = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "OrganizationChartRelationship", objectId = OrganizationChartRelationshipID, body = list(DataObject = body), searchFields = append("OrganizationChartRelationshipID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PositionAssignments
	#'
	#' This function returns a dataframe or json object of PositionAssignments
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PositionAssignments. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PositionAssignments.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PositionAssignment') to get more field paths.
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
	#' @concept Position
	#' @return A list of PositionAssignments
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPositionAssignments <- function(searchConditionsList = NULL, AssignmentID = F, PositionID = F, EmployeeID = F, SalaryCalculationMethodID = F, EmployeePlacementID = F, EntitlementID = F, NextYearIntentID = F, StartDate = F, EndDate = F, ActivePaidDayCount = F, CalendarSeconds = F, TotalPay = F, FormattedCalendarSeconds = F, ContractPaidToDate = F, ContractBalance = F, CurrentScheduledPaidHours = F, AssignmentIdentifier = F, AssignmentCodeIdentifier = F, AttachmentCount = F, TotalRetroPay = F, TotalDockPay = F, AssignmentTimeTrackingGroupID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PercentEmployed = F, TotalStipendAmount = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, StateReportingDistributionsPositionTypeCodes = F, StateReportingDistributionsPositionTypeDescriptions = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, FormattedCalendarSecondsDecimal = F, CurrentScheduledPaidHoursDecimal = F, PositionTypeEmployeeIdentifier = F, PositionDistributionAssignmentRangeIdentifier = F, IsEEOCPrimaryAssignment = F, WorkStartDate = F, WorkEndDate = F, DockedTotalPay = F, AssignmentThirdPartyImportID = F, BaseAssignmentPay = F, SupplementTotalPay = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "Assignment", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PositionAssignment
	#'
	#' This function returns a dataframe or json object of a PositionAssignment
	#' @param PositionAssignmentID The ID of the PositionAssignment to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PositionAssignment. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PositionAssignment.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PositionAssignment') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of PositionAssignment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPositionAssignment <- function(PositionAssignmentID, AssignmentID = F, PositionID = F, EmployeeID = F, SalaryCalculationMethodID = F, EmployeePlacementID = F, EntitlementID = F, NextYearIntentID = F, StartDate = F, EndDate = F, ActivePaidDayCount = F, CalendarSeconds = F, TotalPay = F, FormattedCalendarSeconds = F, ContractPaidToDate = F, ContractBalance = F, CurrentScheduledPaidHours = F, AssignmentIdentifier = F, AssignmentCodeIdentifier = F, AttachmentCount = F, TotalRetroPay = F, TotalDockPay = F, AssignmentTimeTrackingGroupID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PercentEmployed = F, TotalStipendAmount = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, StateReportingDistributionsPositionTypeCodes = F, StateReportingDistributionsPositionTypeDescriptions = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, FormattedCalendarSecondsDecimal = F, CurrentScheduledPaidHoursDecimal = F, PositionTypeEmployeeIdentifier = F, PositionDistributionAssignmentRangeIdentifier = F, IsEEOCPrimaryAssignment = F, WorkStartDate = F, WorkEndDate = F, DockedTotalPay = F, AssignmentThirdPartyImportID = F, BaseAssignmentPay = F, SupplementTotalPay = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PositionAssignmentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "Assignment", objectId = PositionAssignmentID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PositionAssignment
	#'
	#' This function deletes a PositionAssignment
	#' @param PositionAssignmentID The ID of the PositionAssignment to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The PositionAssignmentID of the deleted PositionAssignment.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePositionAssignment <- function(PositionAssignmentID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "Assignment", objectId = PositionAssignmentID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PositionAssignment
	#'
	#' This function creates a PositionAssignment
	#' @param fieldNames The field values to give the created PositionAssignment. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created PositionAssignment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPositionAssignment <- function(PositionID = NULL, EmployeeID = NULL, SalaryCalculationMethodID = NULL, EmployeePlacementID = NULL, EntitlementID = NULL, NextYearIntentID = NULL, AssignmentTimeTrackingGroupID = NULL, IsEEOCPrimaryAssignment = NULL, AssignmentThirdPartyImportID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "Assignment", body = list(DataObject = body), searchFields = append("AssignmentID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PositionAssignment
	#'
	#' This function modifies a PositionAssignment
	#' @param fieldNames The field values to give the modified PositionAssignment. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified PositionAssignment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPositionAssignment <- function(AssignmentID, PositionID = NULL, EmployeeID = NULL, SalaryCalculationMethodID = NULL, EmployeePlacementID = NULL, EntitlementID = NULL, NextYearIntentID = NULL, AssignmentTimeTrackingGroupID = NULL, IsEEOCPrimaryAssignment = NULL, AssignmentThirdPartyImportID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "Assignment", objectId = AssignmentID, body = list(DataObject = body), searchFields = append("AssignmentID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AssignmentDetails
	#'
	#' This function returns a dataframe or json object of AssignmentDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssignmentDetails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssignmentDetails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssignmentDetail') to get more field paths.
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
	#' @concept Position
	#' @return A list of AssignmentDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAssignmentDetails <- function(searchConditionsList = NULL, AssignmentDetailID = F, AssignmentID = F, StartDate = F, EndDate = F, ActivePaidDayCount = F, ActivePaidDayCountOverride = F, SecondsPerDay = F, EnteredFTE = F, CalendarSeconds = F, TotalPay = F, AnnualizedPay = F, HourlyPay = F, DailyPay = F, IsPrimary = F, MatrixID = F, MatrixIDBase = F, StepID = F, EmployeePlacementDetailID = F, EnteredRate = F, StepIDBase = F, EmployeePlacementIDBase = F, PaidFullDays = F, BaseMatrixStep = F, FormattedCalendarSeconds = F, MatrixStep = F, AssignmentDetailIdentifier = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Comment = F, SupplementDailyPayTotal = F, SupplementHourlyPayTotal = F, SupplementAnnualizedPayTotal = F, SupplementPayTotal = F, FormattedSecondsPerDayDecimal = F, FormattedCalendarSecondsDecimal = F, IsOverloaded = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "AssignmentDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AssignmentDetail
	#'
	#' This function returns a dataframe or json object of an AssignmentDetail
	#' @param AssignmentDetailID The ID of the AssignmentDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssignmentDetail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssignmentDetail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssignmentDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of AssignmentDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAssignmentDetail <- function(AssignmentDetailID, AssignmentID = F, StartDate = F, EndDate = F, ActivePaidDayCount = F, ActivePaidDayCountOverride = F, SecondsPerDay = F, EnteredFTE = F, CalendarSeconds = F, TotalPay = F, AnnualizedPay = F, HourlyPay = F, DailyPay = F, IsPrimary = F, MatrixID = F, MatrixIDBase = F, StepID = F, EmployeePlacementDetailID = F, EnteredRate = F, StepIDBase = F, EmployeePlacementIDBase = F, PaidFullDays = F, BaseMatrixStep = F, FormattedCalendarSeconds = F, MatrixStep = F, AssignmentDetailIdentifier = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Comment = F, SupplementDailyPayTotal = F, SupplementHourlyPayTotal = F, SupplementAnnualizedPayTotal = F, SupplementPayTotal = F, FormattedSecondsPerDayDecimal = F, FormattedCalendarSecondsDecimal = F, IsOverloaded = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AssignmentDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "AssignmentDetail", objectId = AssignmentDetailID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AssignmentDetail
	#'
	#' This function deletes an AssignmentDetail
	#' @param AssignmentDetailID The ID of the AssignmentDetail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The AssignmentDetailID of the deleted AssignmentDetail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAssignmentDetail <- function(AssignmentDetailID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "AssignmentDetail", objectId = AssignmentDetailID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AssignmentDetail
	#'
	#' This function creates an AssignmentDetail
	#' @param fieldNames The field values to give the created AssignmentDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created AssignmentDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAssignmentDetail <- function(AssignmentID = NULL, StartDate = NULL, EndDate = NULL, ActivePaidDayCount = NULL, ActivePaidDayCountOverride = NULL, SecondsPerDay = NULL, EnteredFTE = NULL, CalendarSeconds = NULL, TotalPay = NULL, AnnualizedPay = NULL, HourlyPay = NULL, DailyPay = NULL, IsPrimary = NULL, MatrixID = NULL, MatrixIDBase = NULL, StepID = NULL, EmployeePlacementDetailID = NULL, EnteredRate = NULL, StepIDBase = NULL, EmployeePlacementIDBase = NULL, Comment = NULL, SupplementDailyPayTotal = NULL, SupplementHourlyPayTotal = NULL, SupplementAnnualizedPayTotal = NULL, SupplementPayTotal = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "AssignmentDetail", body = list(DataObject = body), searchFields = append("AssignmentDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AssignmentDetail
	#'
	#' This function modifies an AssignmentDetail
	#' @param fieldNames The field values to give the modified AssignmentDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified AssignmentDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAssignmentDetail <- function(AssignmentDetailID, AssignmentID = NULL, StartDate = NULL, EndDate = NULL, ActivePaidDayCount = NULL, ActivePaidDayCountOverride = NULL, SecondsPerDay = NULL, EnteredFTE = NULL, CalendarSeconds = NULL, TotalPay = NULL, AnnualizedPay = NULL, HourlyPay = NULL, DailyPay = NULL, IsPrimary = NULL, MatrixID = NULL, MatrixIDBase = NULL, StepID = NULL, EmployeePlacementDetailID = NULL, EnteredRate = NULL, StepIDBase = NULL, EmployeePlacementIDBase = NULL, Comment = NULL, SupplementDailyPayTotal = NULL, SupplementHourlyPayTotal = NULL, SupplementAnnualizedPayTotal = NULL, SupplementPayTotal = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "AssignmentDetail", objectId = AssignmentDetailID, body = list(DataObject = body), searchFields = append("AssignmentDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PositionDepartments
	#'
	#' This function returns a dataframe or json object of PositionDepartments
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PositionDepartments. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PositionDepartments.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PositionDepartment') to get more field paths.
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
	#' @concept Position
	#' @return A list of PositionDepartments
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPositionDepartments <- function(searchConditionsList = NULL, DepartmentID = F, DistrictID = F, Code = F, Description = F, FiscalYearID = F, CodeDescription = F, DepartmentIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "Department", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PositionDepartment
	#'
	#' This function returns a dataframe or json object of a PositionDepartment
	#' @param PositionDepartmentID The ID of the PositionDepartment to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PositionDepartment. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PositionDepartment.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PositionDepartment') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of PositionDepartment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPositionDepartment <- function(PositionDepartmentID, DepartmentID = F, DistrictID = F, Code = F, Description = F, FiscalYearID = F, CodeDescription = F, DepartmentIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PositionDepartmentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "Department", objectId = PositionDepartmentID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PositionDepartment
	#'
	#' This function deletes a PositionDepartment
	#' @param PositionDepartmentID The ID of the PositionDepartment to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The PositionDepartmentID of the deleted PositionDepartment.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePositionDepartment <- function(PositionDepartmentID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "Department", objectId = PositionDepartmentID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PositionDepartment
	#'
	#' This function creates a PositionDepartment
	#' @param fieldNames The field values to give the created PositionDepartment. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created PositionDepartment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPositionDepartment <- function(DistrictID = NULL, Code = NULL, Description = NULL, FiscalYearID = NULL, DepartmentIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "Department", body = list(DataObject = body), searchFields = append("DepartmentID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PositionDepartment
	#'
	#' This function modifies a PositionDepartment
	#' @param fieldNames The field values to give the modified PositionDepartment. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified PositionDepartment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPositionDepartment <- function(DepartmentID, DistrictID = NULL, Code = NULL, Description = NULL, FiscalYearID = NULL, DepartmentIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "Department", objectId = DepartmentID, body = list(DataObject = body), searchFields = append("DepartmentID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EmployeePlacements
	#'
	#' This function returns a dataframe or json object of EmployeePlacements
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeePlacements. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeePlacements.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeePlacement') to get more field paths.
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
	#' @concept Position
	#' @return A list of EmployeePlacements
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEmployeePlacements <- function(searchConditionsList = NULL, EmployeePlacementID = F, EmployeeID = F, PlacementID = F, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "EmployeePlacement", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EmployeePlacement
	#'
	#' This function returns a dataframe or json object of an EmployeePlacement
	#' @param EmployeePlacementID The ID of the EmployeePlacement to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeePlacement. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeePlacement.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeePlacement') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of EmployeePlacement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEmployeePlacement <- function(EmployeePlacementID, EmployeeID = F, PlacementID = F, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EmployeePlacementID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "EmployeePlacement", objectId = EmployeePlacementID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EmployeePlacement
	#'
	#' This function deletes an EmployeePlacement
	#' @param EmployeePlacementID The ID of the EmployeePlacement to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The EmployeePlacementID of the deleted EmployeePlacement.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEmployeePlacement <- function(EmployeePlacementID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "EmployeePlacement", objectId = EmployeePlacementID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EmployeePlacement
	#'
	#' This function creates an EmployeePlacement
	#' @param fieldNames The field values to give the created EmployeePlacement. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created EmployeePlacement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEmployeePlacement <- function(EmployeeID = NULL, PlacementID = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "EmployeePlacement", body = list(DataObject = body), searchFields = append("EmployeePlacementID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EmployeePlacement
	#'
	#' This function modifies an EmployeePlacement
	#' @param fieldNames The field values to give the modified EmployeePlacement. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified EmployeePlacement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEmployeePlacement <- function(EmployeePlacementID, EmployeeID = NULL, PlacementID = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "EmployeePlacement", objectId = EmployeePlacementID, body = list(DataObject = body), searchFields = append("EmployeePlacementID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EmployeePlacementDetails
	#'
	#' This function returns a dataframe or json object of EmployeePlacementDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeePlacementDetails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeePlacementDetails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeePlacementDetail') to get more field paths.
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
	#' @concept Position
	#' @return A list of EmployeePlacementDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEmployeePlacementDetails <- function(searchConditionsList = NULL, EmployeePlacementDetailID = F, EmployeePlacementID = F, EffectiveDate = F, LaneID = F, StepNumber = F, Credits = F, Placement = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "EmployeePlacementDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EmployeePlacementDetail
	#'
	#' This function returns a dataframe or json object of an EmployeePlacementDetail
	#' @param EmployeePlacementDetailID The ID of the EmployeePlacementDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeePlacementDetail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeePlacementDetail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeePlacementDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of EmployeePlacementDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEmployeePlacementDetail <- function(EmployeePlacementDetailID, EmployeePlacementID = F, EffectiveDate = F, LaneID = F, StepNumber = F, Credits = F, Placement = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EmployeePlacementDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "EmployeePlacementDetail", objectId = EmployeePlacementDetailID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EmployeePlacementDetail
	#'
	#' This function deletes an EmployeePlacementDetail
	#' @param EmployeePlacementDetailID The ID of the EmployeePlacementDetail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The EmployeePlacementDetailID of the deleted EmployeePlacementDetail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEmployeePlacementDetail <- function(EmployeePlacementDetailID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "EmployeePlacementDetail", objectId = EmployeePlacementDetailID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EmployeePlacementDetail
	#'
	#' This function creates an EmployeePlacementDetail
	#' @param fieldNames The field values to give the created EmployeePlacementDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created EmployeePlacementDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEmployeePlacementDetail <- function(EmployeePlacementID = NULL, EffectiveDate = NULL, LaneID = NULL, StepNumber = NULL, Credits = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "EmployeePlacementDetail", body = list(DataObject = body), searchFields = append("EmployeePlacementDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EmployeePlacementDetail
	#'
	#' This function modifies an EmployeePlacementDetail
	#' @param fieldNames The field values to give the modified EmployeePlacementDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified EmployeePlacementDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEmployeePlacementDetail <- function(EmployeePlacementDetailID, EmployeePlacementID = NULL, EffectiveDate = NULL, LaneID = NULL, StepNumber = NULL, Credits = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "EmployeePlacementDetail", objectId = EmployeePlacementDetailID, body = list(DataObject = body), searchFields = append("EmployeePlacementDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List JobTypes
	#'
	#' This function returns a dataframe or json object of JobTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given JobTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the JobTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('JobType') to get more field paths.
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
	#' @concept Position
	#' @return A list of JobTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listJobTypes <- function(searchConditionsList = NULL, JobTypeID = F, DistrictID = F, Code = F, Description = F, FiscalYearID = F, CodeDescription = F, JobTypeIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "JobType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a JobType
	#'
	#' This function returns a dataframe or json object of a JobType
	#' @param JobTypeID The ID of the JobType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given JobType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the JobType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('JobType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of JobType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getJobType <- function(JobTypeID, DistrictID = F, Code = F, Description = F, FiscalYearID = F, CodeDescription = F, JobTypeIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "JobTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "JobType", objectId = JobTypeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a JobType
	#'
	#' This function deletes a JobType
	#' @param JobTypeID The ID of the JobType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The JobTypeID of the deleted JobType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteJobType <- function(JobTypeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "JobType", objectId = JobTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a JobType
	#'
	#' This function creates a JobType
	#' @param fieldNames The field values to give the created JobType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created JobType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createJobType <- function(DistrictID = NULL, Code = NULL, Description = NULL, FiscalYearID = NULL, JobTypeIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "JobType", body = list(DataObject = body), searchFields = append("JobTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a JobType
	#'
	#' This function modifies a JobType
	#' @param fieldNames The field values to give the modified JobType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified JobType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyJobType <- function(JobTypeID, DistrictID = NULL, Code = NULL, Description = NULL, FiscalYearID = NULL, JobTypeIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "JobType", objectId = JobTypeID, body = list(DataObject = body), searchFields = append("JobTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Matrices
	#'
	#' This function returns a dataframe or json object of Matrices
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Matrices. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Matrices.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Matrix') to get more field paths.
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
	#' @concept Position
	#' @return A list of Matrices
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listMatrices <- function(searchConditionsList = NULL, MatrixID = F, DistrictID = F, Code = F, Description = F, FiscalYearID = F, DistrictBase = F, Type = F, CodeDescription = F, MatrixIDClonedFrom = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "Matrix", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Matrix
	#'
	#' This function returns a dataframe or json object of a Matrix
	#' @param MatrixID The ID of the Matrix to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Matrix. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Matrix.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Matrix') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of Matrix
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getMatrix <- function(MatrixID, DistrictID = F, Code = F, Description = F, FiscalYearID = F, DistrictBase = F, Type = F, CodeDescription = F, MatrixIDClonedFrom = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "MatrixID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "Matrix", objectId = MatrixID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Matrix
	#'
	#' This function deletes a Matrix
	#' @param MatrixID The ID of the Matrix to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The MatrixID of the deleted Matrix.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteMatrix <- function(MatrixID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "Matrix", objectId = MatrixID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Matrix
	#'
	#' This function creates a Matrix
	#' @param fieldNames The field values to give the created Matrix. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created Matrix
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createMatrix <- function(DistrictID = NULL, Code = NULL, Description = NULL, FiscalYearID = NULL, DistrictBase = NULL, Type = NULL, MatrixIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "Matrix", body = list(DataObject = body), searchFields = append("MatrixID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Matrix
	#'
	#' This function modifies a Matrix
	#' @param fieldNames The field values to give the modified Matrix. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified Matrix
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyMatrix <- function(MatrixID, DistrictID = NULL, Code = NULL, Description = NULL, FiscalYearID = NULL, DistrictBase = NULL, Type = NULL, MatrixIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "Matrix", objectId = MatrixID, body = list(DataObject = body), searchFields = append("MatrixID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List MatrixLanes
	#'
	#' This function returns a dataframe or json object of MatrixLanes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given MatrixLanes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MatrixLanes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MatrixLane') to get more field paths.
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
	#' @concept Position
	#' @return A list of MatrixLanes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listMatrixLanes <- function(searchConditionsList = NULL, MatrixLaneID = F, MatrixID = F, LaneID = F, RequiredCredits = F, DisplayOrder = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "MatrixLane", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a MatrixLane
	#'
	#' This function returns a dataframe or json object of a MatrixLane
	#' @param MatrixLaneID The ID of the MatrixLane to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given MatrixLane. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MatrixLane.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MatrixLane') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of MatrixLane
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getMatrixLane <- function(MatrixLaneID, MatrixID = F, LaneID = F, RequiredCredits = F, DisplayOrder = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "MatrixLaneID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "MatrixLane", objectId = MatrixLaneID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a MatrixLane
	#'
	#' This function deletes a MatrixLane
	#' @param MatrixLaneID The ID of the MatrixLane to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The MatrixLaneID of the deleted MatrixLane.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteMatrixLane <- function(MatrixLaneID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "MatrixLane", objectId = MatrixLaneID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a MatrixLane
	#'
	#' This function creates a MatrixLane
	#' @param fieldNames The field values to give the created MatrixLane. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created MatrixLane
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createMatrixLane <- function(MatrixID = NULL, LaneID = NULL, RequiredCredits = NULL, DisplayOrder = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "MatrixLane", body = list(DataObject = body), searchFields = append("MatrixLaneID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a MatrixLane
	#'
	#' This function modifies a MatrixLane
	#' @param fieldNames The field values to give the modified MatrixLane. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified MatrixLane
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyMatrixLane <- function(MatrixLaneID, MatrixID = NULL, LaneID = NULL, RequiredCredits = NULL, DisplayOrder = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "MatrixLane", objectId = MatrixLaneID, body = list(DataObject = body), searchFields = append("MatrixLaneID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Placements
	#'
	#' This function returns a dataframe or json object of Placements
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Placements. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Placements.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Placement') to get more field paths.
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
	#' @concept Position
	#' @return A list of Placements
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPlacements <- function(searchConditionsList = NULL, PlacementID = F, DistrictID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "Placement", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Placement
	#'
	#' This function returns a dataframe or json object of a Placement
	#' @param PlacementID The ID of the Placement to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Placement. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Placement.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Placement') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of Placement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPlacement <- function(PlacementID, DistrictID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PlacementID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "Placement", objectId = PlacementID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Placement
	#'
	#' This function deletes a Placement
	#' @param PlacementID The ID of the Placement to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The PlacementID of the deleted Placement.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePlacement <- function(PlacementID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "Placement", objectId = PlacementID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Placement
	#'
	#' This function creates a Placement
	#' @param fieldNames The field values to give the created Placement. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created Placement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPlacement <- function(DistrictID = NULL, Code = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "Placement", body = list(DataObject = body), searchFields = append("PlacementID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Placement
	#'
	#' This function modifies a Placement
	#' @param fieldNames The field values to give the modified Placement. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified Placement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPlacement <- function(PlacementID, DistrictID = NULL, Code = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "Placement", objectId = PlacementID, body = list(DataObject = body), searchFields = append("PlacementID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Positions
	#'
	#' This function returns a dataframe or json object of Positions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Positions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Positions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Position') to get more field paths.
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
	#' @concept Position
	#' @return A list of Positions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPositions <- function(searchConditionsList = NULL, PositionID = F, PositionNumberID = F, DistrictID = F, FiscalYearID = F, PositionTypeID = F, CalendarID = F, StartDate = F, EndDate = F, ActivePaidDayCount = F, FullPaidDays = F, FullPaySecondsPerDay = F, FormattedTotalPaidSeconds = F, TotalPaidSeconds = F, BudgetedFTE = F, PositionGroupID = F, JobTypeID = F, PositionIDClonedFrom = F, ClosingAssignmentFTE = F, VacantClosingAssignmentFTE = F, CurrentAssignmentFTE = F, VacantCurrentAssignmentFTE = F, PositionClosingAssignmentIdentifier = F, PositionCodeIdentifier = F, FormattedPositionCodeEELLabel = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PlanPositionIDClonedFrom = F, FormattedFullPaySecondsPerDayDecimal = F, FormattedTotalPaidSecondsDecimal = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsFTEGroupCodes = F, PositionDistributionsFTEGroupDescriptions = F, PositionDistributionsDepartmentCodes = F, PositionDistributionsDepartmentDescriptions = F, AvailableClosingFTE = F, EmployeeConversionKey = F, HasTimeOffTransactionsAwaitingApproval = F, HasTimeOffTransactionHistory = F, HasTimesheetSubmissionsAwaitingApproval = F, HasTimesheetSubmissionHistory = F, HasExpenseReimbursementsAwaitingApproval = F, HasExpenseReimbursementHistory = F, HasSubstituteTransactionHistory = F, AssignmentThirdPartyImportID = F, BudgetedHoursPerDay = F, ApprovingEmployeeNames = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "Position", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Position
	#'
	#' This function returns a dataframe or json object of a Position
	#' @param PositionID The ID of the Position to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Position. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Position.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Position') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of Position
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPosition <- function(PositionID, PositionNumberID = F, DistrictID = F, FiscalYearID = F, PositionTypeID = F, CalendarID = F, StartDate = F, EndDate = F, ActivePaidDayCount = F, FullPaidDays = F, FullPaySecondsPerDay = F, FormattedTotalPaidSeconds = F, TotalPaidSeconds = F, BudgetedFTE = F, PositionGroupID = F, JobTypeID = F, PositionIDClonedFrom = F, ClosingAssignmentFTE = F, VacantClosingAssignmentFTE = F, CurrentAssignmentFTE = F, VacantCurrentAssignmentFTE = F, PositionClosingAssignmentIdentifier = F, PositionCodeIdentifier = F, FormattedPositionCodeEELLabel = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PlanPositionIDClonedFrom = F, FormattedFullPaySecondsPerDayDecimal = F, FormattedTotalPaidSecondsDecimal = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsFTEGroupCodes = F, PositionDistributionsFTEGroupDescriptions = F, PositionDistributionsDepartmentCodes = F, PositionDistributionsDepartmentDescriptions = F, AvailableClosingFTE = F, EmployeeConversionKey = F, HasTimeOffTransactionsAwaitingApproval = F, HasTimeOffTransactionHistory = F, HasTimesheetSubmissionsAwaitingApproval = F, HasTimesheetSubmissionHistory = F, HasExpenseReimbursementsAwaitingApproval = F, HasExpenseReimbursementHistory = F, HasSubstituteTransactionHistory = F, AssignmentThirdPartyImportID = F, BudgetedHoursPerDay = F, ApprovingEmployeeNames = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PositionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "Position", objectId = PositionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Position
	#'
	#' This function deletes a Position
	#' @param PositionID The ID of the Position to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The PositionID of the deleted Position.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePosition <- function(PositionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "Position", objectId = PositionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Position
	#'
	#' This function creates a Position
	#' @param fieldNames The field values to give the created Position. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created Position
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPosition <- function(PositionNumberID = NULL, DistrictID = NULL, FiscalYearID = NULL, PositionTypeID = NULL, CalendarID = NULL, ActivePaidDayCount = NULL, FullPaidDays = NULL, FullPaySecondsPerDay = NULL, TotalPaidSeconds = NULL, PositionGroupID = NULL, JobTypeID = NULL, PositionIDClonedFrom = NULL, PlanPositionIDClonedFrom = NULL, EmployeeConversionKey = NULL, AssignmentThirdPartyImportID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "Position", body = list(DataObject = body), searchFields = append("PositionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Position
	#'
	#' This function modifies a Position
	#' @param fieldNames The field values to give the modified Position. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified Position
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPosition <- function(PositionID, PositionNumberID = NULL, DistrictID = NULL, FiscalYearID = NULL, PositionTypeID = NULL, CalendarID = NULL, ActivePaidDayCount = NULL, FullPaidDays = NULL, FullPaySecondsPerDay = NULL, TotalPaidSeconds = NULL, PositionGroupID = NULL, JobTypeID = NULL, PositionIDClonedFrom = NULL, PlanPositionIDClonedFrom = NULL, EmployeeConversionKey = NULL, AssignmentThirdPartyImportID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "Position", objectId = PositionID, body = list(DataObject = body), searchFields = append("PositionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PositionGroups
	#'
	#' This function returns a dataframe or json object of PositionGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PositionGroups. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PositionGroups.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PositionGroup') to get more field paths.
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
	#' @concept Position
	#' @return A list of PositionGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPositionGroups <- function(searchConditionsList = NULL, PositionGroupID = F, Code = F, Description = F, DistrictID = F, FiscalYearID = F, CodeDescription = F, PositionGroupIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "PositionGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PositionGroup
	#'
	#' This function returns a dataframe or json object of a PositionGroup
	#' @param PositionGroupID The ID of the PositionGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PositionGroup. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PositionGroup.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PositionGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of PositionGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPositionGroup <- function(PositionGroupID, Code = F, Description = F, DistrictID = F, FiscalYearID = F, CodeDescription = F, PositionGroupIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PositionGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "PositionGroup", objectId = PositionGroupID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PositionGroup
	#'
	#' This function deletes a PositionGroup
	#' @param PositionGroupID The ID of the PositionGroup to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The PositionGroupID of the deleted PositionGroup.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePositionGroup <- function(PositionGroupID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "PositionGroup", objectId = PositionGroupID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PositionGroup
	#'
	#' This function creates a PositionGroup
	#' @param fieldNames The field values to give the created PositionGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created PositionGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPositionGroup <- function(Code = NULL, Description = NULL, DistrictID = NULL, FiscalYearID = NULL, PositionGroupIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "PositionGroup", body = list(DataObject = body), searchFields = append("PositionGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PositionGroup
	#'
	#' This function modifies a PositionGroup
	#' @param fieldNames The field values to give the modified PositionGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified PositionGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPositionGroup <- function(PositionGroupID, Code = NULL, Description = NULL, DistrictID = NULL, FiscalYearID = NULL, PositionGroupIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "PositionGroup", objectId = PositionGroupID, body = list(DataObject = body), searchFields = append("PositionGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List FTEGroups
	#'
	#' This function returns a dataframe or json object of FTEGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given FTEGroups. Defaults to FALSE for all return fields which, for convenience, returns all fields for the FTEGroups.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('FTEGroup') to get more field paths.
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
	#' @concept Position
	#' @return A list of FTEGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listFTEGroups <- function(searchConditionsList = NULL, FTEGroupID = F, Code = F, Description = F, OptimalFTE = F, MaximumFTE = F, FiscalYearID = F, DistrictID = F, TotalPositionFTE = F, CodeDescription = F, ClosingAssignmentFTE = F, FTEGroupIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "FTEGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a FTEGroup
	#'
	#' This function returns a dataframe or json object of a FTEGroup
	#' @param FTEGroupID The ID of the FTEGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given FTEGroup. Defaults to FALSE for all return fields which, for convenience, returns all fields for the FTEGroup.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('FTEGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of FTEGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getFTEGroup <- function(FTEGroupID, Code = F, Description = F, OptimalFTE = F, MaximumFTE = F, FiscalYearID = F, DistrictID = F, TotalPositionFTE = F, CodeDescription = F, ClosingAssignmentFTE = F, FTEGroupIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "FTEGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "FTEGroup", objectId = FTEGroupID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a FTEGroup
	#'
	#' This function deletes a FTEGroup
	#' @param FTEGroupID The ID of the FTEGroup to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The FTEGroupID of the deleted FTEGroup.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteFTEGroup <- function(FTEGroupID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "FTEGroup", objectId = FTEGroupID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a FTEGroup
	#'
	#' This function creates a FTEGroup
	#' @param fieldNames The field values to give the created FTEGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created FTEGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createFTEGroup <- function(Code = NULL, Description = NULL, OptimalFTE = NULL, MaximumFTE = NULL, FiscalYearID = NULL, DistrictID = NULL, FTEGroupIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "FTEGroup", body = list(DataObject = body), searchFields = append("FTEGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a FTEGroup
	#'
	#' This function modifies a FTEGroup
	#' @param fieldNames The field values to give the modified FTEGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified FTEGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyFTEGroup <- function(FTEGroupID, Code = NULL, Description = NULL, OptimalFTE = NULL, MaximumFTE = NULL, FiscalYearID = NULL, DistrictID = NULL, FTEGroupIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "FTEGroup", objectId = FTEGroupID, body = list(DataObject = body), searchFields = append("FTEGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PositionNumbers
	#'
	#' This function returns a dataframe or json object of PositionNumbers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PositionNumbers. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PositionNumbers.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PositionNumber') to get more field paths.
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
	#' @concept Position
	#' @return A list of PositionNumbers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPositionNumbers <- function(searchConditionsList = NULL, PositionNumberID = F, DistrictID = F, Code = F, FullPositionNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsActive = F, HasPositionInFiscalYear = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "PositionNumber", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PositionNumber
	#'
	#' This function returns a dataframe or json object of a PositionNumber
	#' @param PositionNumberID The ID of the PositionNumber to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PositionNumber. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PositionNumber.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PositionNumber') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of PositionNumber
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPositionNumber <- function(PositionNumberID, DistrictID = F, Code = F, FullPositionNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsActive = F, HasPositionInFiscalYear = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PositionNumberID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "PositionNumber", objectId = PositionNumberID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PositionNumber
	#'
	#' This function deletes a PositionNumber
	#' @param PositionNumberID The ID of the PositionNumber to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The PositionNumberID of the deleted PositionNumber.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePositionNumber <- function(PositionNumberID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "PositionNumber", objectId = PositionNumberID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PositionNumber
	#'
	#' This function creates a PositionNumber
	#' @param fieldNames The field values to give the created PositionNumber. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created PositionNumber
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPositionNumber <- function(DistrictID = NULL, Code = NULL, IsActive = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "PositionNumber", body = list(DataObject = body), searchFields = append("PositionNumberID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PositionNumber
	#'
	#' This function modifies a PositionNumber
	#' @param fieldNames The field values to give the modified PositionNumber. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified PositionNumber
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPositionNumber <- function(PositionNumberID, DistrictID = NULL, Code = NULL, IsActive = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "PositionNumber", objectId = PositionNumberID, body = list(DataObject = body), searchFields = append("PositionNumberID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SalaryCalculationMethods
	#'
	#' This function returns a dataframe or json object of SalaryCalculationMethods
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SalaryCalculationMethods. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SalaryCalculationMethods.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SalaryCalculationMethod') to get more field paths.
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
	#' @concept Position
	#' @return A list of SalaryCalculationMethods
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSalaryCalculationMethods <- function(searchConditionsList = NULL, SalaryCalculationMethodID = F, FiscalYearID = F, DistrictID = F, Code = F, Description = F, TimeEntryField = F, SkywardID = F, TotalPayExpression = F, DailyPayExpression = F, HourlyPayExpression = F, AnnualizedPayExpression = F, CodeDescription = F, RenderRestore = F, SalaryCalculationMethodIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AmountType = F, HasMatrix = F, HasSupplement = F, HasEnteredRate = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "SalaryCalculationMethod", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SalaryCalculationMethod
	#'
	#' This function returns a dataframe or json object of a SalaryCalculationMethod
	#' @param SalaryCalculationMethodID The ID of the SalaryCalculationMethod to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SalaryCalculationMethod. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SalaryCalculationMethod.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SalaryCalculationMethod') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of SalaryCalculationMethod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSalaryCalculationMethod <- function(SalaryCalculationMethodID, FiscalYearID = F, DistrictID = F, Code = F, Description = F, TimeEntryField = F, SkywardID = F, TotalPayExpression = F, DailyPayExpression = F, HourlyPayExpression = F, AnnualizedPayExpression = F, CodeDescription = F, RenderRestore = F, SalaryCalculationMethodIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AmountType = F, HasMatrix = F, HasSupplement = F, HasEnteredRate = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SalaryCalculationMethodID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "SalaryCalculationMethod", objectId = SalaryCalculationMethodID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SalaryCalculationMethod
	#'
	#' This function deletes a SalaryCalculationMethod
	#' @param SalaryCalculationMethodID The ID of the SalaryCalculationMethod to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The SalaryCalculationMethodID of the deleted SalaryCalculationMethod.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSalaryCalculationMethod <- function(SalaryCalculationMethodID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "SalaryCalculationMethod", objectId = SalaryCalculationMethodID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SalaryCalculationMethod
	#'
	#' This function creates a SalaryCalculationMethod
	#' @param fieldNames The field values to give the created SalaryCalculationMethod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created SalaryCalculationMethod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSalaryCalculationMethod <- function(FiscalYearID = NULL, DistrictID = NULL, Code = NULL, Description = NULL, TimeEntryField = NULL, SalaryCalculationMethodIDClonedFrom = NULL, AmountType = NULL, HasMatrix = NULL, HasSupplement = NULL, HasEnteredRate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "SalaryCalculationMethod", body = list(DataObject = body), searchFields = append("SalaryCalculationMethodID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SalaryCalculationMethod
	#'
	#' This function modifies a SalaryCalculationMethod
	#' @param fieldNames The field values to give the modified SalaryCalculationMethod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified SalaryCalculationMethod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySalaryCalculationMethod <- function(SalaryCalculationMethodID, FiscalYearID = NULL, DistrictID = NULL, Code = NULL, Description = NULL, TimeEntryField = NULL, SalaryCalculationMethodIDClonedFrom = NULL, AmountType = NULL, HasMatrix = NULL, HasSupplement = NULL, HasEnteredRate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "SalaryCalculationMethod", objectId = SalaryCalculationMethodID, body = list(DataObject = body), searchFields = append("SalaryCalculationMethodID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SalaryCalculationMethodSystems
	#'
	#' This function returns a dataframe or json object of SalaryCalculationMethodSystems
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SalaryCalculationMethodSystems. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SalaryCalculationMethodSystems.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SalaryCalculationMethodSystem') to get more field paths.
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
	#' @concept Position
	#' @return A list of SalaryCalculationMethodSystems
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSalaryCalculationMethodSystems <- function(searchConditionsList = NULL, SalaryCalculationMethodSystemID = F, SkywardCode = F, SkywardDescription = F, SkywardID = F, TimeEntryField = F, TotalPayCalculation = F, DailyPayCalculation = F, HourlyPayCalculation = F, AnnualizedPayCalculation = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AmountType = F, HasMatrix = F, HasSupplement = F, HasEnteredRate = F, SkywardHash = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "SalaryCalculationMethodSystem", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SalaryCalculationMethodSystem
	#'
	#' This function returns a dataframe or json object of a SalaryCalculationMethodSystem
	#' @param SalaryCalculationMethodSystemID The ID of the SalaryCalculationMethodSystem to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SalaryCalculationMethodSystem. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SalaryCalculationMethodSystem.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SalaryCalculationMethodSystem') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of SalaryCalculationMethodSystem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSalaryCalculationMethodSystem <- function(SalaryCalculationMethodSystemID, SkywardCode = F, SkywardDescription = F, SkywardID = F, TimeEntryField = F, TotalPayCalculation = F, DailyPayCalculation = F, HourlyPayCalculation = F, AnnualizedPayCalculation = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AmountType = F, HasMatrix = F, HasSupplement = F, HasEnteredRate = F, SkywardHash = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SalaryCalculationMethodSystemID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "SalaryCalculationMethodSystem", objectId = SalaryCalculationMethodSystemID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SalaryCalculationMethodSystem
	#'
	#' This function deletes a SalaryCalculationMethodSystem
	#' @param SalaryCalculationMethodSystemID The ID of the SalaryCalculationMethodSystem to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The SalaryCalculationMethodSystemID of the deleted SalaryCalculationMethodSystem.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSalaryCalculationMethodSystem <- function(SalaryCalculationMethodSystemID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "SalaryCalculationMethodSystem", objectId = SalaryCalculationMethodSystemID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SalaryCalculationMethodSystem
	#'
	#' This function creates a SalaryCalculationMethodSystem
	#' @param fieldNames The field values to give the created SalaryCalculationMethodSystem. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created SalaryCalculationMethodSystem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSalaryCalculationMethodSystem <- function(SkywardCode = NULL, SkywardDescription = NULL, TimeEntryField = NULL, TotalPayCalculation = NULL, DailyPayCalculation = NULL, HourlyPayCalculation = NULL, AnnualizedPayCalculation = NULL, AmountType = NULL, HasMatrix = NULL, HasSupplement = NULL, HasEnteredRate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "SalaryCalculationMethodSystem", body = list(DataObject = body), searchFields = append("SalaryCalculationMethodSystemID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SalaryCalculationMethodSystem
	#'
	#' This function modifies a SalaryCalculationMethodSystem
	#' @param fieldNames The field values to give the modified SalaryCalculationMethodSystem. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified SalaryCalculationMethodSystem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySalaryCalculationMethodSystem <- function(SalaryCalculationMethodSystemID, SkywardCode = NULL, SkywardDescription = NULL, TimeEntryField = NULL, TotalPayCalculation = NULL, DailyPayCalculation = NULL, HourlyPayCalculation = NULL, AnnualizedPayCalculation = NULL, AmountType = NULL, HasMatrix = NULL, HasSupplement = NULL, HasEnteredRate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "SalaryCalculationMethodSystem", objectId = SalaryCalculationMethodSystemID, body = list(DataObject = body), searchFields = append("SalaryCalculationMethodSystemID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PositionSteps
	#'
	#' This function returns a dataframe or json object of PositionSteps
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PositionSteps. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PositionSteps.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PositionStep') to get more field paths.
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
	#' @concept Position
	#' @return A list of PositionSteps
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPositionSteps <- function(searchConditionsList = NULL, StepID = F, MatrixLaneID = F, StepNumber = F, Value = F, Increment = F, StepNumberOverride = F, LaneIDOverride = F, MatrixIDOverride = F, PlacementIDDefault = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Description = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "Step", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PositionStep
	#'
	#' This function returns a dataframe or json object of a PositionStep
	#' @param PositionStepID The ID of the PositionStep to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PositionStep. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PositionStep.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PositionStep') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of PositionStep
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPositionStep <- function(PositionStepID, StepID = F, MatrixLaneID = F, StepNumber = F, Value = F, Increment = F, StepNumberOverride = F, LaneIDOverride = F, MatrixIDOverride = F, PlacementIDDefault = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Description = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PositionStepID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "Step", objectId = PositionStepID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PositionStep
	#'
	#' This function deletes a PositionStep
	#' @param PositionStepID The ID of the PositionStep to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The PositionStepID of the deleted PositionStep.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePositionStep <- function(PositionStepID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "Step", objectId = PositionStepID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PositionStep
	#'
	#' This function creates a PositionStep
	#' @param fieldNames The field values to give the created PositionStep. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created PositionStep
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPositionStep <- function(MatrixLaneID = NULL, StepNumber = NULL, Value = NULL, Increment = NULL, StepNumberOverride = NULL, LaneIDOverride = NULL, MatrixIDOverride = NULL, PlacementIDDefault = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "Step", body = list(DataObject = body), searchFields = append("StepID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PositionStep
	#'
	#' This function modifies a PositionStep
	#' @param fieldNames The field values to give the modified PositionStep. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified PositionStep
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPositionStep <- function(StepID, MatrixLaneID = NULL, StepNumber = NULL, Value = NULL, Increment = NULL, StepNumberOverride = NULL, LaneIDOverride = NULL, MatrixIDOverride = NULL, PlacementIDDefault = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "Step", objectId = StepID, body = list(DataObject = body), searchFields = append("StepID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Lanes
	#'
	#' This function returns a dataframe or json object of Lanes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Lanes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Lanes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Lane') to get more field paths.
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
	#' @concept Position
	#' @return A list of Lanes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listLanes <- function(searchConditionsList = NULL, LaneID = F, DistrictID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "Lane", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Lane
	#'
	#' This function returns a dataframe or json object of a Lane
	#' @param LaneID The ID of the Lane to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Lane. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Lane.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Lane') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of Lane
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getLane <- function(LaneID, DistrictID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "LaneID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "Lane", objectId = LaneID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Lane
	#'
	#' This function deletes a Lane
	#' @param LaneID The ID of the Lane to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The LaneID of the deleted Lane.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteLane <- function(LaneID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "Lane", objectId = LaneID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Lane
	#'
	#' This function creates a Lane
	#' @param fieldNames The field values to give the created Lane. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created Lane
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createLane <- function(DistrictID = NULL, Code = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "Lane", body = list(DataObject = body), searchFields = append("LaneID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Lane
	#'
	#' This function modifies a Lane
	#' @param fieldNames The field values to give the modified Lane. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified Lane
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyLane <- function(LaneID, DistrictID = NULL, Code = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "Lane", objectId = LaneID, body = list(DataObject = body), searchFields = append("LaneID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PositionSecurityGroups
	#'
	#' This function returns a dataframe or json object of PositionSecurityGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PositionSecurityGroups. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PositionSecurityGroups.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PositionSecurityGroup') to get more field paths.
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
	#' @concept Position
	#' @return A list of PositionSecurityGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPositionSecurityGroups <- function(searchConditionsList = NULL, PositionSecurityGroupID = F, PositionID = F, GroupIDSecurity = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "PositionSecurityGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PositionSecurityGroup
	#'
	#' This function returns a dataframe or json object of a PositionSecurityGroup
	#' @param PositionSecurityGroupID The ID of the PositionSecurityGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PositionSecurityGroup. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PositionSecurityGroup.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PositionSecurityGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of PositionSecurityGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPositionSecurityGroup <- function(PositionSecurityGroupID, PositionID = F, GroupIDSecurity = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PositionSecurityGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "PositionSecurityGroup", objectId = PositionSecurityGroupID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PositionSecurityGroup
	#'
	#' This function deletes a PositionSecurityGroup
	#' @param PositionSecurityGroupID The ID of the PositionSecurityGroup to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The PositionSecurityGroupID of the deleted PositionSecurityGroup.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePositionSecurityGroup <- function(PositionSecurityGroupID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "PositionSecurityGroup", objectId = PositionSecurityGroupID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PositionSecurityGroup
	#'
	#' This function creates a PositionSecurityGroup
	#' @param fieldNames The field values to give the created PositionSecurityGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created PositionSecurityGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPositionSecurityGroup <- function(PositionID = NULL, GroupIDSecurity = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "PositionSecurityGroup", body = list(DataObject = body), searchFields = append("PositionSecurityGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PositionSecurityGroup
	#'
	#' This function modifies a PositionSecurityGroup
	#' @param fieldNames The field values to give the modified PositionSecurityGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified PositionSecurityGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPositionSecurityGroup <- function(PositionSecurityGroupID, PositionID = NULL, GroupIDSecurity = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "PositionSecurityGroup", objectId = PositionSecurityGroupID, body = list(DataObject = body), searchFields = append("PositionSecurityGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempOrganizationChartRelationshipErrors
	#'
	#' This function returns a dataframe or json object of TempOrganizationChartRelationshipErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempOrganizationChartRelationshipErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempOrganizationChartRelationshipErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempOrganizationChartRelationshipError') to get more field paths.
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
	#' @concept Position
	#' @return A list of TempOrganizationChartRelationshipErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempOrganizationChartRelationshipErrors <- function(searchConditionsList = NULL, TempOrganizationChartRelationshipErrorID = F, CurrentEmployee = F, ErrorReason = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeNumber = F, PositionNumber = F, TempOrganizationChartID = F, PositionTypeCode = F, PositionTypeDescription = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempOrganizationChartRelationshipError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempOrganizationChartRelationshipError
	#'
	#' This function returns a dataframe or json object of a TempOrganizationChartRelationshipError
	#' @param TempOrganizationChartRelationshipErrorID The ID of the TempOrganizationChartRelationshipError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempOrganizationChartRelationshipError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempOrganizationChartRelationshipError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempOrganizationChartRelationshipError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of TempOrganizationChartRelationshipError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempOrganizationChartRelationshipError <- function(TempOrganizationChartRelationshipErrorID, CurrentEmployee = F, ErrorReason = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeNumber = F, PositionNumber = F, TempOrganizationChartID = F, PositionTypeCode = F, PositionTypeDescription = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempOrganizationChartRelationshipErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "TempOrganizationChartRelationshipError", objectId = TempOrganizationChartRelationshipErrorID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempOrganizationChartRelationshipError
	#'
	#' This function deletes a TempOrganizationChartRelationshipError
	#' @param TempOrganizationChartRelationshipErrorID The ID of the TempOrganizationChartRelationshipError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The TempOrganizationChartRelationshipErrorID of the deleted TempOrganizationChartRelationshipError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempOrganizationChartRelationshipError <- function(TempOrganizationChartRelationshipErrorID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "TempOrganizationChartRelationshipError", objectId = TempOrganizationChartRelationshipErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempOrganizationChartRelationshipError
	#'
	#' This function creates a TempOrganizationChartRelationshipError
	#' @param fieldNames The field values to give the created TempOrganizationChartRelationshipError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created TempOrganizationChartRelationshipError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempOrganizationChartRelationshipError <- function(CurrentEmployee = NULL, ErrorReason = NULL, EmployeeNumber = NULL, PositionNumber = NULL, TempOrganizationChartID = NULL, PositionTypeCode = NULL, PositionTypeDescription = NULL, PositionDistributionsAssignmentTypeCodes = NULL, PositionDistributionsAssignmentTypeDescriptions = NULL, PositionDistributionsBuildingCodes = NULL, PositionDistributionsBuildingDescriptions = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "TempOrganizationChartRelationshipError", body = list(DataObject = body), searchFields = append("TempOrganizationChartRelationshipErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempOrganizationChartRelationshipError
	#'
	#' This function modifies a TempOrganizationChartRelationshipError
	#' @param fieldNames The field values to give the modified TempOrganizationChartRelationshipError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified TempOrganizationChartRelationshipError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempOrganizationChartRelationshipError <- function(TempOrganizationChartRelationshipErrorID, CurrentEmployee = NULL, ErrorReason = NULL, EmployeeNumber = NULL, PositionNumber = NULL, TempOrganizationChartID = NULL, PositionTypeCode = NULL, PositionTypeDescription = NULL, PositionDistributionsAssignmentTypeCodes = NULL, PositionDistributionsAssignmentTypeDescriptions = NULL, PositionDistributionsBuildingCodes = NULL, PositionDistributionsBuildingDescriptions = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "TempOrganizationChartRelationshipError", objectId = TempOrganizationChartRelationshipErrorID, body = list(DataObject = body), searchFields = append("TempOrganizationChartRelationshipErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempPositionAssignmentPayTypes
	#'
	#' This function returns a dataframe or json object of TempPositionAssignmentPayTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempPositionAssignmentPayTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempPositionAssignmentPayTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempPositionAssignmentPayType') to get more field paths.
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
	#' @concept Position
	#' @return A list of TempPositionAssignmentPayTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempPositionAssignmentPayTypes <- function(searchConditionsList = NULL, TempPositionAssignmentPayTypeID = F, Employee = F, PayTypeCodeDescription = F, AssignmentPayTypeID = F, PositionTypeCodeDescription = F, AssignmentStartDate = F, AssignmentEndDate = F, PositionNumberCode = F, PositionID = F, AssignmentID = F, PositionTypeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeNumber = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempPositionAssignmentPayType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempPositionAssignmentPayType
	#'
	#' This function returns a dataframe or json object of a TempPositionAssignmentPayType
	#' @param TempPositionAssignmentPayTypeID The ID of the TempPositionAssignmentPayType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempPositionAssignmentPayType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempPositionAssignmentPayType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempPositionAssignmentPayType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of TempPositionAssignmentPayType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempPositionAssignmentPayType <- function(TempPositionAssignmentPayTypeID, Employee = F, PayTypeCodeDescription = F, AssignmentPayTypeID = F, PositionTypeCodeDescription = F, AssignmentStartDate = F, AssignmentEndDate = F, PositionNumberCode = F, PositionID = F, AssignmentID = F, PositionTypeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeNumber = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempPositionAssignmentPayTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "TempPositionAssignmentPayType", objectId = TempPositionAssignmentPayTypeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempPositionAssignmentPayType
	#'
	#' This function deletes a TempPositionAssignmentPayType
	#' @param TempPositionAssignmentPayTypeID The ID of the TempPositionAssignmentPayType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The TempPositionAssignmentPayTypeID of the deleted TempPositionAssignmentPayType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempPositionAssignmentPayType <- function(TempPositionAssignmentPayTypeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "TempPositionAssignmentPayType", objectId = TempPositionAssignmentPayTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempPositionAssignmentPayType
	#'
	#' This function creates a TempPositionAssignmentPayType
	#' @param fieldNames The field values to give the created TempPositionAssignmentPayType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created TempPositionAssignmentPayType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempPositionAssignmentPayType <- function(Employee = NULL, PayTypeCodeDescription = NULL, AssignmentPayTypeID = NULL, PositionTypeCodeDescription = NULL, AssignmentStartDate = NULL, AssignmentEndDate = NULL, PositionNumberCode = NULL, PositionID = NULL, AssignmentID = NULL, PositionTypeID = NULL, EmployeeNumber = NULL, PositionDistributionsAssignmentTypeCodes = NULL, PositionDistributionsAssignmentTypeDescriptions = NULL, PositionDistributionsBuildingCodes = NULL, PositionDistributionsBuildingDescriptions = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "TempPositionAssignmentPayType", body = list(DataObject = body), searchFields = append("TempPositionAssignmentPayTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempPositionAssignmentPayType
	#'
	#' This function modifies a TempPositionAssignmentPayType
	#' @param fieldNames The field values to give the modified TempPositionAssignmentPayType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified TempPositionAssignmentPayType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempPositionAssignmentPayType <- function(TempPositionAssignmentPayTypeID, Employee = NULL, PayTypeCodeDescription = NULL, AssignmentPayTypeID = NULL, PositionTypeCodeDescription = NULL, AssignmentStartDate = NULL, AssignmentEndDate = NULL, PositionNumberCode = NULL, PositionID = NULL, AssignmentID = NULL, PositionTypeID = NULL, EmployeeNumber = NULL, PositionDistributionsAssignmentTypeCodes = NULL, PositionDistributionsAssignmentTypeDescriptions = NULL, PositionDistributionsBuildingCodes = NULL, PositionDistributionsBuildingDescriptions = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "TempPositionAssignmentPayType", objectId = TempPositionAssignmentPayTypeID, body = list(DataObject = body), searchFields = append("TempPositionAssignmentPayTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempPositionAssignmentPayTypeErrors
	#'
	#' This function returns a dataframe or json object of TempPositionAssignmentPayTypeErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempPositionAssignmentPayTypeErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempPositionAssignmentPayTypeErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempPositionAssignmentPayTypeError') to get more field paths.
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
	#' @concept Position
	#' @return A list of TempPositionAssignmentPayTypeErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempPositionAssignmentPayTypeErrors <- function(searchConditionsList = NULL, TempPositionAssignmentPayTypeErrorID = F, Employee = F, PositionNumberCode = F, PositionTypeCodeDescription = F, PayTypeCodeDescription = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeNumber = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempPositionAssignmentPayTypeError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempPositionAssignmentPayTypeError
	#'
	#' This function returns a dataframe or json object of a TempPositionAssignmentPayTypeError
	#' @param TempPositionAssignmentPayTypeErrorID The ID of the TempPositionAssignmentPayTypeError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempPositionAssignmentPayTypeError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempPositionAssignmentPayTypeError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempPositionAssignmentPayTypeError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of TempPositionAssignmentPayTypeError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempPositionAssignmentPayTypeError <- function(TempPositionAssignmentPayTypeErrorID, Employee = F, PositionNumberCode = F, PositionTypeCodeDescription = F, PayTypeCodeDescription = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeNumber = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempPositionAssignmentPayTypeErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "TempPositionAssignmentPayTypeError", objectId = TempPositionAssignmentPayTypeErrorID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempPositionAssignmentPayTypeError
	#'
	#' This function deletes a TempPositionAssignmentPayTypeError
	#' @param TempPositionAssignmentPayTypeErrorID The ID of the TempPositionAssignmentPayTypeError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The TempPositionAssignmentPayTypeErrorID of the deleted TempPositionAssignmentPayTypeError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempPositionAssignmentPayTypeError <- function(TempPositionAssignmentPayTypeErrorID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "TempPositionAssignmentPayTypeError", objectId = TempPositionAssignmentPayTypeErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempPositionAssignmentPayTypeError
	#'
	#' This function creates a TempPositionAssignmentPayTypeError
	#' @param fieldNames The field values to give the created TempPositionAssignmentPayTypeError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created TempPositionAssignmentPayTypeError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempPositionAssignmentPayTypeError <- function(Employee = NULL, PositionNumberCode = NULL, PositionTypeCodeDescription = NULL, PayTypeCodeDescription = NULL, Message = NULL, EmployeeNumber = NULL, PositionDistributionsAssignmentTypeCodes = NULL, PositionDistributionsAssignmentTypeDescriptions = NULL, PositionDistributionsBuildingCodes = NULL, PositionDistributionsBuildingDescriptions = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "TempPositionAssignmentPayTypeError", body = list(DataObject = body), searchFields = append("TempPositionAssignmentPayTypeErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempPositionAssignmentPayTypeError
	#'
	#' This function modifies a TempPositionAssignmentPayTypeError
	#' @param fieldNames The field values to give the modified TempPositionAssignmentPayTypeError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified TempPositionAssignmentPayTypeError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempPositionAssignmentPayTypeError <- function(TempPositionAssignmentPayTypeErrorID, Employee = NULL, PositionNumberCode = NULL, PositionTypeCodeDescription = NULL, PayTypeCodeDescription = NULL, Message = NULL, EmployeeNumber = NULL, PositionDistributionsAssignmentTypeCodes = NULL, PositionDistributionsAssignmentTypeDescriptions = NULL, PositionDistributionsBuildingCodes = NULL, PositionDistributionsBuildingDescriptions = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "TempPositionAssignmentPayTypeError", objectId = TempPositionAssignmentPayTypeErrorID, body = list(DataObject = body), searchFields = append("TempPositionAssignmentPayTypeErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AssignmentTypeStateReportingDistributionSummaries
	#'
	#' This function returns a dataframe or json object of AssignmentTypeStateReportingDistributionSummaries
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssignmentTypeStateReportingDistributionSummaries. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssignmentTypeStateReportingDistributionSummaries.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssignmentTypeStateReportingDistributionSummary') to get more field paths.
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
	#' @concept Position
	#' @return A list of AssignmentTypeStateReportingDistributionSummaries
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAssignmentTypeStateReportingDistributionSummaries <- function(searchConditionsList = NULL, StateReportingDistributionIDFirst = F, PositionTypeID = F, AssignmentTypeID = F, CurrentFTE = F, ClosingAssignmentFTE = F, VacantClosingAssignmentFTE = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "AssignmentTypeStateReportingDistributionSummary", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AssignmentTypeStateReportingDistributionSummary
	#'
	#' This function returns a dataframe or json object of an AssignmentTypeStateReportingDistributionSummary
	#' @param AssignmentTypeStateReportingDistributionSummaryID The ID of the AssignmentTypeStateReportingDistributionSummary to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssignmentTypeStateReportingDistributionSummary. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssignmentTypeStateReportingDistributionSummary.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssignmentTypeStateReportingDistributionSummary') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of AssignmentTypeStateReportingDistributionSummary
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAssignmentTypeStateReportingDistributionSummary <- function(AssignmentTypeStateReportingDistributionSummaryID, StateReportingDistributionIDFirst = F, PositionTypeID = F, AssignmentTypeID = F, CurrentFTE = F, ClosingAssignmentFTE = F, VacantClosingAssignmentFTE = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AssignmentTypeStateReportingDistributionSummaryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "AssignmentTypeStateReportingDistributionSummary", objectId = AssignmentTypeStateReportingDistributionSummaryID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AssignmentTypeStateReportingDistributionSummary
	#'
	#' This function deletes an AssignmentTypeStateReportingDistributionSummary
	#' @param AssignmentTypeStateReportingDistributionSummaryID The ID of the AssignmentTypeStateReportingDistributionSummary to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The AssignmentTypeStateReportingDistributionSummaryID of the deleted AssignmentTypeStateReportingDistributionSummary.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAssignmentTypeStateReportingDistributionSummary <- function(AssignmentTypeStateReportingDistributionSummaryID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "AssignmentTypeStateReportingDistributionSummary", objectId = AssignmentTypeStateReportingDistributionSummaryID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AssignmentTypeStateReportingDistributionSummary
	#'
	#' This function creates an AssignmentTypeStateReportingDistributionSummary
	#' @param fieldNames The field values to give the created AssignmentTypeStateReportingDistributionSummary. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created AssignmentTypeStateReportingDistributionSummary
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAssignmentTypeStateReportingDistributionSummary <- function(PositionTypeID = NULL, AssignmentTypeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "AssignmentTypeStateReportingDistributionSummary", body = list(DataObject = body), searchFields = append("AssignmentTypeStateReportingDistributionSummaryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AssignmentTypeStateReportingDistributionSummary
	#'
	#' This function modifies an AssignmentTypeStateReportingDistributionSummary
	#' @param fieldNames The field values to give the modified AssignmentTypeStateReportingDistributionSummary. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified AssignmentTypeStateReportingDistributionSummary
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAssignmentTypeStateReportingDistributionSummary <- function(AssignmentTypeStateReportingDistributionSummaryID, PositionTypeID = NULL, AssignmentTypeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "AssignmentTypeStateReportingDistributionSummary", objectId = AssignmentTypeStateReportingDistributionSummaryID, body = list(DataObject = body), searchFields = append("AssignmentTypeStateReportingDistributionSummaryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List BuildingStateReportingDistributionSummaries
	#'
	#' This function returns a dataframe or json object of BuildingStateReportingDistributionSummaries
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BuildingStateReportingDistributionSummaries. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BuildingStateReportingDistributionSummaries.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BuildingStateReportingDistributionSummary') to get more field paths.
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
	#' @concept Position
	#' @return A list of BuildingStateReportingDistributionSummaries
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listBuildingStateReportingDistributionSummaries <- function(searchConditionsList = NULL, StateReportingDistributionIDFirst = F, PositionTypeID = F, BuildingID = F, CurrentFTE = F, ClosingAssignmentFTE = F, VacantClosingAssignmentFTE = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "BuildingStateReportingDistributionSummary", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a BuildingStateReportingDistributionSummary
	#'
	#' This function returns a dataframe or json object of a BuildingStateReportingDistributionSummary
	#' @param BuildingStateReportingDistributionSummaryID The ID of the BuildingStateReportingDistributionSummary to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BuildingStateReportingDistributionSummary. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BuildingStateReportingDistributionSummary.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BuildingStateReportingDistributionSummary') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of BuildingStateReportingDistributionSummary
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getBuildingStateReportingDistributionSummary <- function(BuildingStateReportingDistributionSummaryID, StateReportingDistributionIDFirst = F, PositionTypeID = F, BuildingID = F, CurrentFTE = F, ClosingAssignmentFTE = F, VacantClosingAssignmentFTE = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "BuildingStateReportingDistributionSummaryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "BuildingStateReportingDistributionSummary", objectId = BuildingStateReportingDistributionSummaryID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a BuildingStateReportingDistributionSummary
	#'
	#' This function deletes a BuildingStateReportingDistributionSummary
	#' @param BuildingStateReportingDistributionSummaryID The ID of the BuildingStateReportingDistributionSummary to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The BuildingStateReportingDistributionSummaryID of the deleted BuildingStateReportingDistributionSummary.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteBuildingStateReportingDistributionSummary <- function(BuildingStateReportingDistributionSummaryID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "BuildingStateReportingDistributionSummary", objectId = BuildingStateReportingDistributionSummaryID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a BuildingStateReportingDistributionSummary
	#'
	#' This function creates a BuildingStateReportingDistributionSummary
	#' @param fieldNames The field values to give the created BuildingStateReportingDistributionSummary. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created BuildingStateReportingDistributionSummary
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createBuildingStateReportingDistributionSummary <- function(PositionTypeID = NULL, BuildingID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "BuildingStateReportingDistributionSummary", body = list(DataObject = body), searchFields = append("BuildingStateReportingDistributionSummaryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a BuildingStateReportingDistributionSummary
	#'
	#' This function modifies a BuildingStateReportingDistributionSummary
	#' @param fieldNames The field values to give the modified BuildingStateReportingDistributionSummary. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified BuildingStateReportingDistributionSummary
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyBuildingStateReportingDistributionSummary <- function(BuildingStateReportingDistributionSummaryID, PositionTypeID = NULL, BuildingID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "BuildingStateReportingDistributionSummary", objectId = BuildingStateReportingDistributionSummaryID, body = list(DataObject = body), searchFields = append("BuildingStateReportingDistributionSummaryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempStateReportingDistributions
	#'
	#' This function returns a dataframe or json object of TempStateReportingDistributions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStateReportingDistributions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStateReportingDistributions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStateReportingDistribution') to get more field paths.
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
	#' @concept Position
	#' @return A list of TempStateReportingDistributions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStateReportingDistributions <- function(searchConditionsList = NULL, TempStateReportingDistributionID = F, AssignmentID = F, PositionTypeID = F, AssignmentTypeID = F, BuildingID = F, DistrictNumber = F, StateDistrictTypeCodeMNID = F, StateDistrictTypeCodeMNCode = F, FileFolderNumber = F, STARSchoolNumber = F, SocialSecurityNumber = F, StateSTARAssignmentCodeMNID = F, StateSTARAssignmentCodeMNCode = F, StateSTARGradeLevelMNID = F, StateSTARGradeLevelMNCode = F, StateSTARModeOfTeachingMNID = F, StateSTARModeOfTeachingMNCode = F, STARPeriodsPerWeek = F, STARLengthOfPeriod = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeID = F, EmployeeFullNameFML = F, EmployeeFullNameLFM = F, AssignmentCodeIdentifier = F, PositionTypeCodeDescription = F, AssignmentTypeCodeDescription = F, BuildingCodeDescription = F, Percentage = F, StateReportingDistributionID = F, SaveChanges = F, StateEISBilingualLanguageILID = F, StateEISGradeLevelAssignmentILID = F, StateEISPositionTimeFrameILID = F, StateEISNonCertifiedCategoryILIDOverride = F, STAROutOfDistrictAssignment = F, WISEStaffWorkingLEANumber = F, WISEStaffBilingualProgram = F, WISEStaffLongTermSub = F, WISEStaffAlternativeEducationProgram = F, WISEStaffSubcontractedIndividual = F, WISEStaffGradeLevelValue = F, StateEISEd360RoleILID = F, StateFTE = F, StateFTEOverride = F, EmployeeNumber = F, EISWorkload = F, StateRetirementJobCategoryWIID = F, IsPR1500Teacher = F, IsPR1500ESL = F, IsPR1500SpecialEd = F, PR1500TeacherGradeRange = F, StatePR1500AssignmentTypeTXID = F, PIMSAssignmentStartDate = F, StateREPAssignmentMIIDOverride = F, REPNumberOfClassesTaught = F, REPGradeLevels = F, REPEducationalSettings = F, EnableMOSISFiscalAgentCountyDistrictOverride = F, MOSISFiscalAgentCountyDistrictOverride = F, StateMOSISPositionMOIDOverride = F, StateMOSISCTEProgramTypeMOIDOverride = F, StatePSRSPEERSPositionMOIDOverride = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempStateReportingDistribution", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempStateReportingDistribution
	#'
	#' This function returns a dataframe or json object of a TempStateReportingDistribution
	#' @param TempStateReportingDistributionID The ID of the TempStateReportingDistribution to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStateReportingDistribution. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStateReportingDistribution.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStateReportingDistribution') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of TempStateReportingDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempStateReportingDistribution <- function(TempStateReportingDistributionID, AssignmentID = F, PositionTypeID = F, AssignmentTypeID = F, BuildingID = F, DistrictNumber = F, StateDistrictTypeCodeMNID = F, StateDistrictTypeCodeMNCode = F, FileFolderNumber = F, STARSchoolNumber = F, SocialSecurityNumber = F, StateSTARAssignmentCodeMNID = F, StateSTARAssignmentCodeMNCode = F, StateSTARGradeLevelMNID = F, StateSTARGradeLevelMNCode = F, StateSTARModeOfTeachingMNID = F, StateSTARModeOfTeachingMNCode = F, STARPeriodsPerWeek = F, STARLengthOfPeriod = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeID = F, EmployeeFullNameFML = F, EmployeeFullNameLFM = F, AssignmentCodeIdentifier = F, PositionTypeCodeDescription = F, AssignmentTypeCodeDescription = F, BuildingCodeDescription = F, Percentage = F, StateReportingDistributionID = F, SaveChanges = F, StateEISBilingualLanguageILID = F, StateEISGradeLevelAssignmentILID = F, StateEISPositionTimeFrameILID = F, StateEISNonCertifiedCategoryILIDOverride = F, STAROutOfDistrictAssignment = F, WISEStaffWorkingLEANumber = F, WISEStaffBilingualProgram = F, WISEStaffLongTermSub = F, WISEStaffAlternativeEducationProgram = F, WISEStaffSubcontractedIndividual = F, WISEStaffGradeLevelValue = F, StateEISEd360RoleILID = F, StateFTE = F, StateFTEOverride = F, EmployeeNumber = F, EISWorkload = F, StateRetirementJobCategoryWIID = F, IsPR1500Teacher = F, IsPR1500ESL = F, IsPR1500SpecialEd = F, PR1500TeacherGradeRange = F, StatePR1500AssignmentTypeTXID = F, PIMSAssignmentStartDate = F, StateREPAssignmentMIIDOverride = F, REPNumberOfClassesTaught = F, REPGradeLevels = F, REPEducationalSettings = F, EnableMOSISFiscalAgentCountyDistrictOverride = F, MOSISFiscalAgentCountyDistrictOverride = F, StateMOSISPositionMOIDOverride = F, StateMOSISCTEProgramTypeMOIDOverride = F, StatePSRSPEERSPositionMOIDOverride = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempStateReportingDistributionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "TempStateReportingDistribution", objectId = TempStateReportingDistributionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempStateReportingDistribution
	#'
	#' This function deletes a TempStateReportingDistribution
	#' @param TempStateReportingDistributionID The ID of the TempStateReportingDistribution to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The TempStateReportingDistributionID of the deleted TempStateReportingDistribution.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempStateReportingDistribution <- function(TempStateReportingDistributionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "TempStateReportingDistribution", objectId = TempStateReportingDistributionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempStateReportingDistribution
	#'
	#' This function creates a TempStateReportingDistribution
	#' @param fieldNames The field values to give the created TempStateReportingDistribution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created TempStateReportingDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempStateReportingDistribution <- function(AssignmentID = NULL, PositionTypeID = NULL, AssignmentTypeID = NULL, BuildingID = NULL, DistrictNumber = NULL, StateDistrictTypeCodeMNID = NULL, StateDistrictTypeCodeMNCode = NULL, FileFolderNumber = NULL, STARSchoolNumber = NULL, SocialSecurityNumber = NULL, StateSTARAssignmentCodeMNID = NULL, StateSTARAssignmentCodeMNCode = NULL, StateSTARGradeLevelMNID = NULL, StateSTARGradeLevelMNCode = NULL, StateSTARModeOfTeachingMNID = NULL, StateSTARModeOfTeachingMNCode = NULL, STARPeriodsPerWeek = NULL, STARLengthOfPeriod = NULL, EmployeeID = NULL, EmployeeFullNameFML = NULL, EmployeeFullNameLFM = NULL, AssignmentCodeIdentifier = NULL, PositionTypeCodeDescription = NULL, AssignmentTypeCodeDescription = NULL, BuildingCodeDescription = NULL, Percentage = NULL, StateReportingDistributionID = NULL, SaveChanges = NULL, StateEISBilingualLanguageILID = NULL, StateEISGradeLevelAssignmentILID = NULL, StateEISPositionTimeFrameILID = NULL, StateEISNonCertifiedCategoryILIDOverride = NULL, STAROutOfDistrictAssignment = NULL, WISEStaffWorkingLEANumber = NULL, WISEStaffBilingualProgram = NULL, WISEStaffLongTermSub = NULL, WISEStaffAlternativeEducationProgram = NULL, WISEStaffSubcontractedIndividual = NULL, WISEStaffGradeLevelValue = NULL, StateEISEd360RoleILID = NULL, StateFTE = NULL, StateFTEOverride = NULL, EmployeeNumber = NULL, EISWorkload = NULL, StateRetirementJobCategoryWIID = NULL, IsPR1500Teacher = NULL, IsPR1500ESL = NULL, IsPR1500SpecialEd = NULL, PR1500TeacherGradeRange = NULL, StatePR1500AssignmentTypeTXID = NULL, PIMSAssignmentStartDate = NULL, StateREPAssignmentMIIDOverride = NULL, REPNumberOfClassesTaught = NULL, REPGradeLevels = NULL, REPEducationalSettings = NULL, EnableMOSISFiscalAgentCountyDistrictOverride = NULL, MOSISFiscalAgentCountyDistrictOverride = NULL, StateMOSISPositionMOIDOverride = NULL, StateMOSISCTEProgramTypeMOIDOverride = NULL, StatePSRSPEERSPositionMOIDOverride = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "TempStateReportingDistribution", body = list(DataObject = body), searchFields = append("TempStateReportingDistributionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempStateReportingDistribution
	#'
	#' This function modifies a TempStateReportingDistribution
	#' @param fieldNames The field values to give the modified TempStateReportingDistribution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified TempStateReportingDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempStateReportingDistribution <- function(TempStateReportingDistributionID, AssignmentID = NULL, PositionTypeID = NULL, AssignmentTypeID = NULL, BuildingID = NULL, DistrictNumber = NULL, StateDistrictTypeCodeMNID = NULL, StateDistrictTypeCodeMNCode = NULL, FileFolderNumber = NULL, STARSchoolNumber = NULL, SocialSecurityNumber = NULL, StateSTARAssignmentCodeMNID = NULL, StateSTARAssignmentCodeMNCode = NULL, StateSTARGradeLevelMNID = NULL, StateSTARGradeLevelMNCode = NULL, StateSTARModeOfTeachingMNID = NULL, StateSTARModeOfTeachingMNCode = NULL, STARPeriodsPerWeek = NULL, STARLengthOfPeriod = NULL, EmployeeID = NULL, EmployeeFullNameFML = NULL, EmployeeFullNameLFM = NULL, AssignmentCodeIdentifier = NULL, PositionTypeCodeDescription = NULL, AssignmentTypeCodeDescription = NULL, BuildingCodeDescription = NULL, Percentage = NULL, StateReportingDistributionID = NULL, SaveChanges = NULL, StateEISBilingualLanguageILID = NULL, StateEISGradeLevelAssignmentILID = NULL, StateEISPositionTimeFrameILID = NULL, StateEISNonCertifiedCategoryILIDOverride = NULL, STAROutOfDistrictAssignment = NULL, WISEStaffWorkingLEANumber = NULL, WISEStaffBilingualProgram = NULL, WISEStaffLongTermSub = NULL, WISEStaffAlternativeEducationProgram = NULL, WISEStaffSubcontractedIndividual = NULL, WISEStaffGradeLevelValue = NULL, StateEISEd360RoleILID = NULL, StateFTE = NULL, StateFTEOverride = NULL, EmployeeNumber = NULL, EISWorkload = NULL, StateRetirementJobCategoryWIID = NULL, IsPR1500Teacher = NULL, IsPR1500ESL = NULL, IsPR1500SpecialEd = NULL, PR1500TeacherGradeRange = NULL, StatePR1500AssignmentTypeTXID = NULL, PIMSAssignmentStartDate = NULL, StateREPAssignmentMIIDOverride = NULL, REPNumberOfClassesTaught = NULL, REPGradeLevels = NULL, REPEducationalSettings = NULL, EnableMOSISFiscalAgentCountyDistrictOverride = NULL, MOSISFiscalAgentCountyDistrictOverride = NULL, StateMOSISPositionMOIDOverride = NULL, StateMOSISCTEProgramTypeMOIDOverride = NULL, StatePSRSPEERSPositionMOIDOverride = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "TempStateReportingDistribution", objectId = TempStateReportingDistributionID, body = list(DataObject = body), searchFields = append("TempStateReportingDistributionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PositionTempExceptions
	#'
	#' This function returns a dataframe or json object of PositionTempExceptions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PositionTempExceptions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PositionTempExceptions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PositionTempException') to get more field paths.
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
	#' @concept Position
	#' @return A list of PositionTempExceptions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPositionTempExceptions <- function(searchConditionsList = NULL, TempExceptionID = F, Message = F, AssignmentIdentifier = F, AssignmentDetailIdentifier = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeName = F, EmployeeNumber = F, PositionTypeCode = F, PositionTypeDescription = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempException", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PositionTempException
	#'
	#' This function returns a dataframe or json object of a PositionTempException
	#' @param PositionTempExceptionID The ID of the PositionTempException to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PositionTempException. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PositionTempException.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PositionTempException') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of PositionTempException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPositionTempException <- function(PositionTempExceptionID, TempExceptionID = F, Message = F, AssignmentIdentifier = F, AssignmentDetailIdentifier = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeName = F, EmployeeNumber = F, PositionTypeCode = F, PositionTypeDescription = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PositionTempExceptionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "TempException", objectId = PositionTempExceptionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PositionTempException
	#'
	#' This function deletes a PositionTempException
	#' @param PositionTempExceptionID The ID of the PositionTempException to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The PositionTempExceptionID of the deleted PositionTempException.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePositionTempException <- function(PositionTempExceptionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "TempException", objectId = PositionTempExceptionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PositionTempException
	#'
	#' This function creates a PositionTempException
	#' @param fieldNames The field values to give the created PositionTempException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created PositionTempException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPositionTempException <- function(Message = NULL, AssignmentIdentifier = NULL, AssignmentDetailIdentifier = NULL, EmployeeName = NULL, EmployeeNumber = NULL, PositionTypeCode = NULL, PositionTypeDescription = NULL, PositionDistributionsAssignmentTypeCodes = NULL, PositionDistributionsAssignmentTypeDescriptions = NULL, PositionDistributionsBuildingCodes = NULL, PositionDistributionsBuildingDescriptions = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "TempException", body = list(DataObject = body), searchFields = append("TempExceptionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PositionTempException
	#'
	#' This function modifies a PositionTempException
	#' @param fieldNames The field values to give the modified PositionTempException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified PositionTempException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPositionTempException <- function(TempExceptionID, Message = NULL, AssignmentIdentifier = NULL, AssignmentDetailIdentifier = NULL, EmployeeName = NULL, EmployeeNumber = NULL, PositionTypeCode = NULL, PositionTypeDescription = NULL, PositionDistributionsAssignmentTypeCodes = NULL, PositionDistributionsAssignmentTypeDescriptions = NULL, PositionDistributionsBuildingCodes = NULL, PositionDistributionsBuildingDescriptions = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "TempException", objectId = TempExceptionID, body = list(DataObject = body), searchFields = append("TempExceptionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempStateReportingDistributionExceptions
	#'
	#' This function returns a dataframe or json object of TempStateReportingDistributionExceptions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStateReportingDistributionExceptions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStateReportingDistributionExceptions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStateReportingDistributionException') to get more field paths.
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
	#' @concept Position
	#' @return A list of TempStateReportingDistributionExceptions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStateReportingDistributionExceptions <- function(searchConditionsList = NULL, TempStateReportingDistributionExceptionID = F, EmployeeID = F, EmployeeFullNameFML = F, EmployeeFullNameLFM = F, AssignmentID = F, AssignmentCodeIdentifier = F, PositionTypeID = F, PositionTypeCodeDescription = F, AssignmentTypeID = F, AssignmentTypeCodeDescription = F, BuildingID = F, BuildingCodeDescription = F, Message = F, RetrievalSource = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsFatal = F, EmployeeNumber = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempStateReportingDistributionException", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempStateReportingDistributionException
	#'
	#' This function returns a dataframe or json object of a TempStateReportingDistributionException
	#' @param TempStateReportingDistributionExceptionID The ID of the TempStateReportingDistributionException to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStateReportingDistributionException. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStateReportingDistributionException.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStateReportingDistributionException') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of TempStateReportingDistributionException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempStateReportingDistributionException <- function(TempStateReportingDistributionExceptionID, EmployeeID = F, EmployeeFullNameFML = F, EmployeeFullNameLFM = F, AssignmentID = F, AssignmentCodeIdentifier = F, PositionTypeID = F, PositionTypeCodeDescription = F, AssignmentTypeID = F, AssignmentTypeCodeDescription = F, BuildingID = F, BuildingCodeDescription = F, Message = F, RetrievalSource = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsFatal = F, EmployeeNumber = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempStateReportingDistributionExceptionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "TempStateReportingDistributionException", objectId = TempStateReportingDistributionExceptionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempStateReportingDistributionException
	#'
	#' This function deletes a TempStateReportingDistributionException
	#' @param TempStateReportingDistributionExceptionID The ID of the TempStateReportingDistributionException to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The TempStateReportingDistributionExceptionID of the deleted TempStateReportingDistributionException.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempStateReportingDistributionException <- function(TempStateReportingDistributionExceptionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "TempStateReportingDistributionException", objectId = TempStateReportingDistributionExceptionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempStateReportingDistributionException
	#'
	#' This function creates a TempStateReportingDistributionException
	#' @param fieldNames The field values to give the created TempStateReportingDistributionException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created TempStateReportingDistributionException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempStateReportingDistributionException <- function(EmployeeID = NULL, EmployeeFullNameFML = NULL, EmployeeFullNameLFM = NULL, AssignmentID = NULL, AssignmentCodeIdentifier = NULL, PositionTypeID = NULL, PositionTypeCodeDescription = NULL, AssignmentTypeID = NULL, AssignmentTypeCodeDescription = NULL, BuildingID = NULL, BuildingCodeDescription = NULL, Message = NULL, RetrievalSource = NULL, IsFatal = NULL, EmployeeNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "TempStateReportingDistributionException", body = list(DataObject = body), searchFields = append("TempStateReportingDistributionExceptionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempStateReportingDistributionException
	#'
	#' This function modifies a TempStateReportingDistributionException
	#' @param fieldNames The field values to give the modified TempStateReportingDistributionException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified TempStateReportingDistributionException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempStateReportingDistributionException <- function(TempStateReportingDistributionExceptionID, EmployeeID = NULL, EmployeeFullNameFML = NULL, EmployeeFullNameLFM = NULL, AssignmentID = NULL, AssignmentCodeIdentifier = NULL, PositionTypeID = NULL, PositionTypeCodeDescription = NULL, AssignmentTypeID = NULL, AssignmentTypeCodeDescription = NULL, BuildingID = NULL, BuildingCodeDescription = NULL, Message = NULL, RetrievalSource = NULL, IsFatal = NULL, EmployeeNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "TempStateReportingDistributionException", objectId = TempStateReportingDistributionExceptionID, body = list(DataObject = body), searchFields = append("TempStateReportingDistributionExceptionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PositionConfigDistricts
	#'
	#' This function returns a dataframe or json object of PositionConfigDistricts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PositionConfigDistricts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PositionConfigDistricts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PositionConfigDistrict') to get more field paths.
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
	#' @concept Position
	#' @return A list of PositionConfigDistricts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPositionConfigDistricts <- function(searchConditionsList = NULL, ConfigDistrictID = F, DistrictID = F, AssignmentOverlapThreshold = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "ConfigDistrict", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PositionConfigDistrict
	#'
	#' This function returns a dataframe or json object of a PositionConfigDistrict
	#' @param PositionConfigDistrictID The ID of the PositionConfigDistrict to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PositionConfigDistrict. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PositionConfigDistrict.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PositionConfigDistrict') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of PositionConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPositionConfigDistrict <- function(PositionConfigDistrictID, ConfigDistrictID = F, DistrictID = F, AssignmentOverlapThreshold = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PositionConfigDistrictID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "ConfigDistrict", objectId = PositionConfigDistrictID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PositionConfigDistrict
	#'
	#' This function deletes a PositionConfigDistrict
	#' @param PositionConfigDistrictID The ID of the PositionConfigDistrict to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The PositionConfigDistrictID of the deleted PositionConfigDistrict.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePositionConfigDistrict <- function(PositionConfigDistrictID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "ConfigDistrict", objectId = PositionConfigDistrictID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PositionConfigDistrict
	#'
	#' This function creates a PositionConfigDistrict
	#' @param fieldNames The field values to give the created PositionConfigDistrict. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created PositionConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPositionConfigDistrict <- function(DistrictID = NULL, AssignmentOverlapThreshold = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "ConfigDistrict", body = list(DataObject = body), searchFields = append("ConfigDistrictID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PositionConfigDistrict
	#'
	#' This function modifies a PositionConfigDistrict
	#' @param fieldNames The field values to give the modified PositionConfigDistrict. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified PositionConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPositionConfigDistrict <- function(ConfigDistrictID, DistrictID = NULL, AssignmentOverlapThreshold = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "ConfigDistrict", objectId = ConfigDistrictID, body = list(DataObject = body), searchFields = append("ConfigDistrictID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SupplementCalculationMethods
	#'
	#' This function returns a dataframe or json object of SupplementCalculationMethods
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SupplementCalculationMethods. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SupplementCalculationMethods.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SupplementCalculationMethod') to get more field paths.
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
	#' @concept Position
	#' @return A list of SupplementCalculationMethods
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSupplementCalculationMethods <- function(searchConditionsList = NULL, SupplementCalculationMethodID = F, FiscalYearID = F, DistrictID = F, Code = F, Description = F, SkywardID = F, SkywardIDClonedFrom = F, SupplementCalculationMethodIDClonedFrom = F, AnnualPayExpression = F, HourlyPayExpression = F, DailyPayExpression = F, IsSystemLoaded = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalPayExpression = F, RenderRestore = F, SkywardHash = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "SupplementCalculationMethod", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SupplementCalculationMethod
	#'
	#' This function returns a dataframe or json object of a SupplementCalculationMethod
	#' @param SupplementCalculationMethodID The ID of the SupplementCalculationMethod to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SupplementCalculationMethod. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SupplementCalculationMethod.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SupplementCalculationMethod') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of SupplementCalculationMethod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSupplementCalculationMethod <- function(SupplementCalculationMethodID, FiscalYearID = F, DistrictID = F, Code = F, Description = F, SkywardID = F, SkywardIDClonedFrom = F, SupplementCalculationMethodIDClonedFrom = F, AnnualPayExpression = F, HourlyPayExpression = F, DailyPayExpression = F, IsSystemLoaded = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalPayExpression = F, RenderRestore = F, SkywardHash = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SupplementCalculationMethodID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "SupplementCalculationMethod", objectId = SupplementCalculationMethodID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SupplementCalculationMethod
	#'
	#' This function deletes a SupplementCalculationMethod
	#' @param SupplementCalculationMethodID The ID of the SupplementCalculationMethod to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The SupplementCalculationMethodID of the deleted SupplementCalculationMethod.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSupplementCalculationMethod <- function(SupplementCalculationMethodID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "SupplementCalculationMethod", objectId = SupplementCalculationMethodID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SupplementCalculationMethod
	#'
	#' This function creates a SupplementCalculationMethod
	#' @param fieldNames The field values to give the created SupplementCalculationMethod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created SupplementCalculationMethod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSupplementCalculationMethod <- function(FiscalYearID = NULL, DistrictID = NULL, Code = NULL, Description = NULL, SkywardIDClonedFrom = NULL, SupplementCalculationMethodIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "SupplementCalculationMethod", body = list(DataObject = body), searchFields = append("SupplementCalculationMethodID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SupplementCalculationMethod
	#'
	#' This function modifies a SupplementCalculationMethod
	#' @param fieldNames The field values to give the modified SupplementCalculationMethod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified SupplementCalculationMethod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySupplementCalculationMethod <- function(SupplementCalculationMethodID, FiscalYearID = NULL, DistrictID = NULL, Code = NULL, Description = NULL, SkywardIDClonedFrom = NULL, SupplementCalculationMethodIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "SupplementCalculationMethod", objectId = SupplementCalculationMethodID, body = list(DataObject = body), searchFields = append("SupplementCalculationMethodID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SupplementTypes
	#'
	#' This function returns a dataframe or json object of SupplementTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SupplementTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SupplementTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SupplementType') to get more field paths.
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
	#' @concept Position
	#' @return A list of SupplementTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSupplementTypes <- function(searchConditionsList = NULL, SupplementTypeID = F, Code = F, Description = F, DistrictID = F, FiscalYearID = F, AmountType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SupplementCalculationMethodID = F, CodeDescription = F, TotalPlanPositionSupplements = F, SupplementTypeIDClonedFrom = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "SupplementType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SupplementType
	#'
	#' This function returns a dataframe or json object of a SupplementType
	#' @param SupplementTypeID The ID of the SupplementType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SupplementType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SupplementType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SupplementType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of SupplementType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSupplementType <- function(SupplementTypeID, Code = F, Description = F, DistrictID = F, FiscalYearID = F, AmountType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SupplementCalculationMethodID = F, CodeDescription = F, TotalPlanPositionSupplements = F, SupplementTypeIDClonedFrom = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SupplementTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "SupplementType", objectId = SupplementTypeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SupplementType
	#'
	#' This function deletes a SupplementType
	#' @param SupplementTypeID The ID of the SupplementType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The SupplementTypeID of the deleted SupplementType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSupplementType <- function(SupplementTypeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "SupplementType", objectId = SupplementTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SupplementType
	#'
	#' This function creates a SupplementType
	#' @param fieldNames The field values to give the created SupplementType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created SupplementType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSupplementType <- function(Code = NULL, Description = NULL, DistrictID = NULL, FiscalYearID = NULL, AmountType = NULL, SupplementCalculationMethodID = NULL, SupplementTypeIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "SupplementType", body = list(DataObject = body), searchFields = append("SupplementTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SupplementType
	#'
	#' This function modifies a SupplementType
	#' @param fieldNames The field values to give the modified SupplementType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified SupplementType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySupplementType <- function(SupplementTypeID, Code = NULL, Description = NULL, DistrictID = NULL, FiscalYearID = NULL, AmountType = NULL, SupplementCalculationMethodID = NULL, SupplementTypeIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "SupplementType", objectId = SupplementTypeID, body = list(DataObject = body), searchFields = append("SupplementTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Supplements
	#'
	#' This function returns a dataframe or json object of Supplements
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Supplements. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Supplements.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Supplement') to get more field paths.
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
	#' @concept Position
	#' @return A list of Supplements
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSupplements <- function(searchConditionsList = NULL, SupplementID = F, SupplementTypeID = F, AssignmentDetailID = F, EnteredRate = F, DailyPay = F, HourlyPay = F, TotalPay = F, AnnualizedPay = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SupplementIDClonedFrom = F, PlanPositionSupplementID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "Supplement", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Supplement
	#'
	#' This function returns a dataframe or json object of a Supplement
	#' @param SupplementID The ID of the Supplement to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Supplement. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Supplement.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Supplement') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of Supplement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSupplement <- function(SupplementID, SupplementTypeID = F, AssignmentDetailID = F, EnteredRate = F, DailyPay = F, HourlyPay = F, TotalPay = F, AnnualizedPay = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SupplementIDClonedFrom = F, PlanPositionSupplementID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SupplementID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "Supplement", objectId = SupplementID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Supplement
	#'
	#' This function deletes a Supplement
	#' @param SupplementID The ID of the Supplement to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The SupplementID of the deleted Supplement.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSupplement <- function(SupplementID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "Supplement", objectId = SupplementID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Supplement
	#'
	#' This function creates a Supplement
	#' @param fieldNames The field values to give the created Supplement. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created Supplement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSupplement <- function(SupplementTypeID = NULL, AssignmentDetailID = NULL, EnteredRate = NULL, DailyPay = NULL, HourlyPay = NULL, TotalPay = NULL, AnnualizedPay = NULL, SupplementIDClonedFrom = NULL, PlanPositionSupplementID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "Supplement", body = list(DataObject = body), searchFields = append("SupplementID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Supplement
	#'
	#' This function modifies a Supplement
	#' @param fieldNames The field values to give the modified Supplement. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified Supplement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySupplement <- function(SupplementID, SupplementTypeID = NULL, AssignmentDetailID = NULL, EnteredRate = NULL, DailyPay = NULL, HourlyPay = NULL, TotalPay = NULL, AnnualizedPay = NULL, SupplementIDClonedFrom = NULL, PlanPositionSupplementID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "Supplement", objectId = SupplementID, body = list(DataObject = body), searchFields = append("SupplementID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempMassUpdatePositions
	#'
	#' This function returns a dataframe or json object of TempMassUpdatePositions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempMassUpdatePositions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempMassUpdatePositions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempMassUpdatePosition') to get more field paths.
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
	#' @concept Position
	#' @return A list of TempMassUpdatePositions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempMassUpdatePositions <- function(searchConditionsList = NULL, TempMassUpdatePositionID = F, PositionID = F, EmployeeFullNameLFM = F, PositionNumberCode = F, PositionNumberID = F, OldPositionTypeCodeDescription = F, NewPositionTypeCodeDescription = F, PositionTypeID = F, PositionStartDate = F, PositionEndDate = F, OldFullPaySecondsPerDay = F, NewFullPaySecondsPerDay = F, OldPositionGroupCodeDescription = F, NewPositionGroupCodeDescription = F, PositionGroupID = F, OldJobTypeCodeDescription = F, NewJobTypeCodeDescription = F, JobTypeID = F, TotalPaidSeconds = F, ActivePaidDayCount = F, FullPaidDays = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OldStateTRSBaseLaneTXCodeDescription = F, NewStateTRSBaseLaneTXCodeDescription = F, StateTRSBaseLaneTXID = F, EmployeeNumber = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, PositionDistributionsFTEGroupCodes = F, PositionDistributionsFTEGroupDescriptions = F, ErrorCount = F, OldCalendarCodeDescription = F, NewCalendarCodeDescription = F, CalendarID = F, CanBeUpdated = F, ExceptionMessage = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempMassUpdatePosition", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempMassUpdatePosition
	#'
	#' This function returns a dataframe or json object of a TempMassUpdatePosition
	#' @param TempMassUpdatePositionID The ID of the TempMassUpdatePosition to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempMassUpdatePosition. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempMassUpdatePosition.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempMassUpdatePosition') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of TempMassUpdatePosition
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempMassUpdatePosition <- function(TempMassUpdatePositionID, PositionID = F, EmployeeFullNameLFM = F, PositionNumberCode = F, PositionNumberID = F, OldPositionTypeCodeDescription = F, NewPositionTypeCodeDescription = F, PositionTypeID = F, PositionStartDate = F, PositionEndDate = F, OldFullPaySecondsPerDay = F, NewFullPaySecondsPerDay = F, OldPositionGroupCodeDescription = F, NewPositionGroupCodeDescription = F, PositionGroupID = F, OldJobTypeCodeDescription = F, NewJobTypeCodeDescription = F, JobTypeID = F, TotalPaidSeconds = F, ActivePaidDayCount = F, FullPaidDays = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OldStateTRSBaseLaneTXCodeDescription = F, NewStateTRSBaseLaneTXCodeDescription = F, StateTRSBaseLaneTXID = F, EmployeeNumber = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, PositionDistributionsFTEGroupCodes = F, PositionDistributionsFTEGroupDescriptions = F, ErrorCount = F, OldCalendarCodeDescription = F, NewCalendarCodeDescription = F, CalendarID = F, CanBeUpdated = F, ExceptionMessage = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempMassUpdatePositionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "TempMassUpdatePosition", objectId = TempMassUpdatePositionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempMassUpdatePosition
	#'
	#' This function deletes a TempMassUpdatePosition
	#' @param TempMassUpdatePositionID The ID of the TempMassUpdatePosition to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The TempMassUpdatePositionID of the deleted TempMassUpdatePosition.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempMassUpdatePosition <- function(TempMassUpdatePositionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "TempMassUpdatePosition", objectId = TempMassUpdatePositionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempMassUpdatePosition
	#'
	#' This function creates a TempMassUpdatePosition
	#' @param fieldNames The field values to give the created TempMassUpdatePosition. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created TempMassUpdatePosition
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempMassUpdatePosition <- function(PositionID = NULL, EmployeeFullNameLFM = NULL, PositionNumberCode = NULL, PositionNumberID = NULL, OldPositionTypeCodeDescription = NULL, NewPositionTypeCodeDescription = NULL, PositionTypeID = NULL, PositionStartDate = NULL, PositionEndDate = NULL, OldFullPaySecondsPerDay = NULL, NewFullPaySecondsPerDay = NULL, OldPositionGroupCodeDescription = NULL, NewPositionGroupCodeDescription = NULL, PositionGroupID = NULL, OldJobTypeCodeDescription = NULL, NewJobTypeCodeDescription = NULL, JobTypeID = NULL, TotalPaidSeconds = NULL, ActivePaidDayCount = NULL, FullPaidDays = NULL, OldStateTRSBaseLaneTXCodeDescription = NULL, NewStateTRSBaseLaneTXCodeDescription = NULL, StateTRSBaseLaneTXID = NULL, EmployeeNumber = NULL, PositionDistributionsAssignmentTypeCodes = NULL, PositionDistributionsAssignmentTypeDescriptions = NULL, PositionDistributionsBuildingCodes = NULL, PositionDistributionsBuildingDescriptions = NULL, PositionDistributionsFTEGroupCodes = NULL, PositionDistributionsFTEGroupDescriptions = NULL, ErrorCount = NULL, OldCalendarCodeDescription = NULL, NewCalendarCodeDescription = NULL, CalendarID = NULL, CanBeUpdated = NULL, ExceptionMessage = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "TempMassUpdatePosition", body = list(DataObject = body), searchFields = append("TempMassUpdatePositionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempMassUpdatePosition
	#'
	#' This function modifies a TempMassUpdatePosition
	#' @param fieldNames The field values to give the modified TempMassUpdatePosition. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified TempMassUpdatePosition
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempMassUpdatePosition <- function(TempMassUpdatePositionID, PositionID = NULL, EmployeeFullNameLFM = NULL, PositionNumberCode = NULL, PositionNumberID = NULL, OldPositionTypeCodeDescription = NULL, NewPositionTypeCodeDescription = NULL, PositionTypeID = NULL, PositionStartDate = NULL, PositionEndDate = NULL, OldFullPaySecondsPerDay = NULL, NewFullPaySecondsPerDay = NULL, OldPositionGroupCodeDescription = NULL, NewPositionGroupCodeDescription = NULL, PositionGroupID = NULL, OldJobTypeCodeDescription = NULL, NewJobTypeCodeDescription = NULL, JobTypeID = NULL, TotalPaidSeconds = NULL, ActivePaidDayCount = NULL, FullPaidDays = NULL, OldStateTRSBaseLaneTXCodeDescription = NULL, NewStateTRSBaseLaneTXCodeDescription = NULL, StateTRSBaseLaneTXID = NULL, EmployeeNumber = NULL, PositionDistributionsAssignmentTypeCodes = NULL, PositionDistributionsAssignmentTypeDescriptions = NULL, PositionDistributionsBuildingCodes = NULL, PositionDistributionsBuildingDescriptions = NULL, PositionDistributionsFTEGroupCodes = NULL, PositionDistributionsFTEGroupDescriptions = NULL, ErrorCount = NULL, OldCalendarCodeDescription = NULL, NewCalendarCodeDescription = NULL, CalendarID = NULL, CanBeUpdated = NULL, ExceptionMessage = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "TempMassUpdatePosition", objectId = TempMassUpdatePositionID, body = list(DataObject = body), searchFields = append("TempMassUpdatePositionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempMassUpdatePositionAssignmentDetails
	#'
	#' This function returns a dataframe or json object of TempMassUpdatePositionAssignmentDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempMassUpdatePositionAssignmentDetails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempMassUpdatePositionAssignmentDetails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempMassUpdatePositionAssignmentDetail') to get more field paths.
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
	#' @concept Position
	#' @return A list of TempMassUpdatePositionAssignmentDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempMassUpdatePositionAssignmentDetails <- function(searchConditionsList = NULL, TempMassUpdatePositionAssignmentDetailID = F, AssignmentID = F, EmployeeFullNameLFM = F, OriginalAssignmentDetailID = F, AssignmentDetailStartDate = F, AssignmentDetailEndDate = F, OldTotalPay = F, NewTotalPay = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeNumber = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempMassUpdatePositionAssignmentDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempMassUpdatePositionAssignmentDetail
	#'
	#' This function returns a dataframe or json object of a TempMassUpdatePositionAssignmentDetail
	#' @param TempMassUpdatePositionAssignmentDetailID The ID of the TempMassUpdatePositionAssignmentDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempMassUpdatePositionAssignmentDetail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempMassUpdatePositionAssignmentDetail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempMassUpdatePositionAssignmentDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of TempMassUpdatePositionAssignmentDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempMassUpdatePositionAssignmentDetail <- function(TempMassUpdatePositionAssignmentDetailID, AssignmentID = F, EmployeeFullNameLFM = F, OriginalAssignmentDetailID = F, AssignmentDetailStartDate = F, AssignmentDetailEndDate = F, OldTotalPay = F, NewTotalPay = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeNumber = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempMassUpdatePositionAssignmentDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "TempMassUpdatePositionAssignmentDetail", objectId = TempMassUpdatePositionAssignmentDetailID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempMassUpdatePositionAssignmentDetail
	#'
	#' This function deletes a TempMassUpdatePositionAssignmentDetail
	#' @param TempMassUpdatePositionAssignmentDetailID The ID of the TempMassUpdatePositionAssignmentDetail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The TempMassUpdatePositionAssignmentDetailID of the deleted TempMassUpdatePositionAssignmentDetail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempMassUpdatePositionAssignmentDetail <- function(TempMassUpdatePositionAssignmentDetailID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "TempMassUpdatePositionAssignmentDetail", objectId = TempMassUpdatePositionAssignmentDetailID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempMassUpdatePositionAssignmentDetail
	#'
	#' This function creates a TempMassUpdatePositionAssignmentDetail
	#' @param fieldNames The field values to give the created TempMassUpdatePositionAssignmentDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created TempMassUpdatePositionAssignmentDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempMassUpdatePositionAssignmentDetail <- function(AssignmentID = NULL, EmployeeFullNameLFM = NULL, OriginalAssignmentDetailID = NULL, AssignmentDetailStartDate = NULL, AssignmentDetailEndDate = NULL, OldTotalPay = NULL, NewTotalPay = NULL, EmployeeNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "TempMassUpdatePositionAssignmentDetail", body = list(DataObject = body), searchFields = append("TempMassUpdatePositionAssignmentDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempMassUpdatePositionAssignmentDetail
	#'
	#' This function modifies a TempMassUpdatePositionAssignmentDetail
	#' @param fieldNames The field values to give the modified TempMassUpdatePositionAssignmentDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified TempMassUpdatePositionAssignmentDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempMassUpdatePositionAssignmentDetail <- function(TempMassUpdatePositionAssignmentDetailID, AssignmentID = NULL, EmployeeFullNameLFM = NULL, OriginalAssignmentDetailID = NULL, AssignmentDetailStartDate = NULL, AssignmentDetailEndDate = NULL, OldTotalPay = NULL, NewTotalPay = NULL, EmployeeNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "TempMassUpdatePositionAssignmentDetail", objectId = TempMassUpdatePositionAssignmentDetailID, body = list(DataObject = body), searchFields = append("TempMassUpdatePositionAssignmentDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempSupplements
	#'
	#' This function returns a dataframe or json object of TempSupplements
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempSupplements. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempSupplements.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempSupplement') to get more field paths.
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
	#' @concept Position
	#' @return A list of TempSupplements
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempSupplements <- function(searchConditionsList = NULL, TempSupplementID = F, AssignmentDetailID = F, EmployeeFullNameLFM = F, SalaryCalculationMethodCodeDescription = F, OldAssignmentDetailHourlyPay = F, OldAssignmentDetailDailyPay = F, OldAssignmentDetailAnnualizedPay = F, OldAssignmentDetailTotalPay = F, NewAssignmentDetailHourlyPay = F, NewAssignmentDetailDailyPay = F, NewAssignmentDetailAnnualizedPay = F, NewAssignmentDetailTotalPay = F, SupplementHourlyPay = F, SupplementDailyPay = F, SupplementAnnualizedPay = F, SupplementPayTotal = F, StartDate = F, EndDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SupplementID = F, OldSupplementHourlyPay = F, OldSupplementDailyPay = F, OldSupplementAnnualizedPay = F, OldSupplementPayTotal = F, OldSupplementEnteredRate = F, NewSupplementEnteredRate = F, EnteredRate = F, TotalPay = F, HourlyPay = F, DailyPay = F, AnnualizedPay = F, SupplementType = F, EmployeeNumber = F, PositionNumberCode = F, PositionTypeCode = F, PositionTypeDescription = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, ErrorCount = F, HasErrors = F, TempPositionID = F, SupplementTypeID = F, PlanPositionSupplementID = F, IsMinimumSalarySupplement = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempSupplement", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempSupplement
	#'
	#' This function returns a dataframe or json object of a TempSupplement
	#' @param TempSupplementID The ID of the TempSupplement to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempSupplement. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempSupplement.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempSupplement') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of TempSupplement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempSupplement <- function(TempSupplementID, AssignmentDetailID = F, EmployeeFullNameLFM = F, SalaryCalculationMethodCodeDescription = F, OldAssignmentDetailHourlyPay = F, OldAssignmentDetailDailyPay = F, OldAssignmentDetailAnnualizedPay = F, OldAssignmentDetailTotalPay = F, NewAssignmentDetailHourlyPay = F, NewAssignmentDetailDailyPay = F, NewAssignmentDetailAnnualizedPay = F, NewAssignmentDetailTotalPay = F, SupplementHourlyPay = F, SupplementDailyPay = F, SupplementAnnualizedPay = F, SupplementPayTotal = F, StartDate = F, EndDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SupplementID = F, OldSupplementHourlyPay = F, OldSupplementDailyPay = F, OldSupplementAnnualizedPay = F, OldSupplementPayTotal = F, OldSupplementEnteredRate = F, NewSupplementEnteredRate = F, EnteredRate = F, TotalPay = F, HourlyPay = F, DailyPay = F, AnnualizedPay = F, SupplementType = F, EmployeeNumber = F, PositionNumberCode = F, PositionTypeCode = F, PositionTypeDescription = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, ErrorCount = F, HasErrors = F, TempPositionID = F, SupplementTypeID = F, PlanPositionSupplementID = F, IsMinimumSalarySupplement = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempSupplementID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "TempSupplement", objectId = TempSupplementID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempSupplement
	#'
	#' This function deletes a TempSupplement
	#' @param TempSupplementID The ID of the TempSupplement to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The TempSupplementID of the deleted TempSupplement.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempSupplement <- function(TempSupplementID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "TempSupplement", objectId = TempSupplementID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempSupplement
	#'
	#' This function creates a TempSupplement
	#' @param fieldNames The field values to give the created TempSupplement. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created TempSupplement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempSupplement <- function(AssignmentDetailID = NULL, EmployeeFullNameLFM = NULL, SalaryCalculationMethodCodeDescription = NULL, OldAssignmentDetailHourlyPay = NULL, OldAssignmentDetailDailyPay = NULL, OldAssignmentDetailAnnualizedPay = NULL, OldAssignmentDetailTotalPay = NULL, NewAssignmentDetailHourlyPay = NULL, NewAssignmentDetailDailyPay = NULL, NewAssignmentDetailAnnualizedPay = NULL, NewAssignmentDetailTotalPay = NULL, SupplementHourlyPay = NULL, SupplementDailyPay = NULL, SupplementAnnualizedPay = NULL, SupplementPayTotal = NULL, StartDate = NULL, EndDate = NULL, SupplementID = NULL, OldSupplementHourlyPay = NULL, OldSupplementDailyPay = NULL, OldSupplementAnnualizedPay = NULL, OldSupplementPayTotal = NULL, OldSupplementEnteredRate = NULL, NewSupplementEnteredRate = NULL, EnteredRate = NULL, TotalPay = NULL, HourlyPay = NULL, DailyPay = NULL, AnnualizedPay = NULL, SupplementType = NULL, EmployeeNumber = NULL, PositionNumberCode = NULL, PositionTypeCode = NULL, PositionTypeDescription = NULL, PositionDistributionsAssignmentTypeCodes = NULL, PositionDistributionsAssignmentTypeDescriptions = NULL, PositionDistributionsBuildingCodes = NULL, PositionDistributionsBuildingDescriptions = NULL, ErrorCount = NULL, HasErrors = NULL, TempPositionID = NULL, SupplementTypeID = NULL, PlanPositionSupplementID = NULL, IsMinimumSalarySupplement = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "TempSupplement", body = list(DataObject = body), searchFields = append("TempSupplementID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempSupplement
	#'
	#' This function modifies a TempSupplement
	#' @param fieldNames The field values to give the modified TempSupplement. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified TempSupplement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempSupplement <- function(TempSupplementID, AssignmentDetailID = NULL, EmployeeFullNameLFM = NULL, SalaryCalculationMethodCodeDescription = NULL, OldAssignmentDetailHourlyPay = NULL, OldAssignmentDetailDailyPay = NULL, OldAssignmentDetailAnnualizedPay = NULL, OldAssignmentDetailTotalPay = NULL, NewAssignmentDetailHourlyPay = NULL, NewAssignmentDetailDailyPay = NULL, NewAssignmentDetailAnnualizedPay = NULL, NewAssignmentDetailTotalPay = NULL, SupplementHourlyPay = NULL, SupplementDailyPay = NULL, SupplementAnnualizedPay = NULL, SupplementPayTotal = NULL, StartDate = NULL, EndDate = NULL, SupplementID = NULL, OldSupplementHourlyPay = NULL, OldSupplementDailyPay = NULL, OldSupplementAnnualizedPay = NULL, OldSupplementPayTotal = NULL, OldSupplementEnteredRate = NULL, NewSupplementEnteredRate = NULL, EnteredRate = NULL, TotalPay = NULL, HourlyPay = NULL, DailyPay = NULL, AnnualizedPay = NULL, SupplementType = NULL, EmployeeNumber = NULL, PositionNumberCode = NULL, PositionTypeCode = NULL, PositionTypeDescription = NULL, PositionDistributionsAssignmentTypeCodes = NULL, PositionDistributionsAssignmentTypeDescriptions = NULL, PositionDistributionsBuildingCodes = NULL, PositionDistributionsBuildingDescriptions = NULL, ErrorCount = NULL, HasErrors = NULL, TempPositionID = NULL, SupplementTypeID = NULL, PlanPositionSupplementID = NULL, IsMinimumSalarySupplement = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "TempSupplement", objectId = TempSupplementID, body = list(DataObject = body), searchFields = append("TempSupplementID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempSupplementErrors
	#'
	#' This function returns a dataframe or json object of TempSupplementErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempSupplementErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempSupplementErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempSupplementError') to get more field paths.
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
	#' @concept Position
	#' @return A list of TempSupplementErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempSupplementErrors <- function(searchConditionsList = NULL, TempSupplementErrorID = F, EmployeeFullNameLFM = F, SalaryCalculationMethodCodeDescription = F, OldAssignmentDetailHourlyPay = F, OldAssignmentDetailDailyPay = F, OldAssignmentDetailAnnualizedPay = F, OldAssignmentDetailTotalPay = F, NewAssignmentDetailHourlyPay = F, NewAssignmentDetailDailyPay = F, NewAssignmentDetailAnnualizedPay = F, NewAssignmentDetailTotalPay = F, SupplementHourlyPay = F, SupplementDailyPay = F, SupplementAnnualizedPay = F, SupplementPayTotal = F, StartDate = F, EndDate = F, ErrorDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OldSupplementHourlyPay = F, OldSupplementDailyPay = F, OldSupplementAnnualizedPay = F, OldSupplementPayTotal = F, OldSupplementEnteredRate = F, NewSupplementEnteredRate = F, EmployeeNumber = F, PositionNumberCode = F, PositionTypeCode = F, PositionTypeDescription = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, TempSupplementID = F, ErrorNumber = F, IsMinimumSalarySupplement = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempSupplementError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempSupplementError
	#'
	#' This function returns a dataframe or json object of a TempSupplementError
	#' @param TempSupplementErrorID The ID of the TempSupplementError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempSupplementError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempSupplementError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempSupplementError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of TempSupplementError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempSupplementError <- function(TempSupplementErrorID, EmployeeFullNameLFM = F, SalaryCalculationMethodCodeDescription = F, OldAssignmentDetailHourlyPay = F, OldAssignmentDetailDailyPay = F, OldAssignmentDetailAnnualizedPay = F, OldAssignmentDetailTotalPay = F, NewAssignmentDetailHourlyPay = F, NewAssignmentDetailDailyPay = F, NewAssignmentDetailAnnualizedPay = F, NewAssignmentDetailTotalPay = F, SupplementHourlyPay = F, SupplementDailyPay = F, SupplementAnnualizedPay = F, SupplementPayTotal = F, StartDate = F, EndDate = F, ErrorDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OldSupplementHourlyPay = F, OldSupplementDailyPay = F, OldSupplementAnnualizedPay = F, OldSupplementPayTotal = F, OldSupplementEnteredRate = F, NewSupplementEnteredRate = F, EmployeeNumber = F, PositionNumberCode = F, PositionTypeCode = F, PositionTypeDescription = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, TempSupplementID = F, ErrorNumber = F, IsMinimumSalarySupplement = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempSupplementErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "TempSupplementError", objectId = TempSupplementErrorID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempSupplementError
	#'
	#' This function deletes a TempSupplementError
	#' @param TempSupplementErrorID The ID of the TempSupplementError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The TempSupplementErrorID of the deleted TempSupplementError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempSupplementError <- function(TempSupplementErrorID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "TempSupplementError", objectId = TempSupplementErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempSupplementError
	#'
	#' This function creates a TempSupplementError
	#' @param fieldNames The field values to give the created TempSupplementError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created TempSupplementError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempSupplementError <- function(EmployeeFullNameLFM = NULL, SalaryCalculationMethodCodeDescription = NULL, OldAssignmentDetailHourlyPay = NULL, OldAssignmentDetailDailyPay = NULL, OldAssignmentDetailAnnualizedPay = NULL, OldAssignmentDetailTotalPay = NULL, NewAssignmentDetailHourlyPay = NULL, NewAssignmentDetailDailyPay = NULL, NewAssignmentDetailAnnualizedPay = NULL, NewAssignmentDetailTotalPay = NULL, SupplementHourlyPay = NULL, SupplementDailyPay = NULL, SupplementAnnualizedPay = NULL, SupplementPayTotal = NULL, StartDate = NULL, EndDate = NULL, ErrorDescription = NULL, OldSupplementHourlyPay = NULL, OldSupplementDailyPay = NULL, OldSupplementAnnualizedPay = NULL, OldSupplementPayTotal = NULL, OldSupplementEnteredRate = NULL, NewSupplementEnteredRate = NULL, EmployeeNumber = NULL, PositionNumberCode = NULL, PositionTypeCode = NULL, PositionTypeDescription = NULL, PositionDistributionsAssignmentTypeCodes = NULL, PositionDistributionsAssignmentTypeDescriptions = NULL, PositionDistributionsBuildingCodes = NULL, PositionDistributionsBuildingDescriptions = NULL, TempSupplementID = NULL, ErrorNumber = NULL, IsMinimumSalarySupplement = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "TempSupplementError", body = list(DataObject = body), searchFields = append("TempSupplementErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempSupplementError
	#'
	#' This function modifies a TempSupplementError
	#' @param fieldNames The field values to give the modified TempSupplementError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified TempSupplementError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempSupplementError <- function(TempSupplementErrorID, EmployeeFullNameLFM = NULL, SalaryCalculationMethodCodeDescription = NULL, OldAssignmentDetailHourlyPay = NULL, OldAssignmentDetailDailyPay = NULL, OldAssignmentDetailAnnualizedPay = NULL, OldAssignmentDetailTotalPay = NULL, NewAssignmentDetailHourlyPay = NULL, NewAssignmentDetailDailyPay = NULL, NewAssignmentDetailAnnualizedPay = NULL, NewAssignmentDetailTotalPay = NULL, SupplementHourlyPay = NULL, SupplementDailyPay = NULL, SupplementAnnualizedPay = NULL, SupplementPayTotal = NULL, StartDate = NULL, EndDate = NULL, ErrorDescription = NULL, OldSupplementHourlyPay = NULL, OldSupplementDailyPay = NULL, OldSupplementAnnualizedPay = NULL, OldSupplementPayTotal = NULL, OldSupplementEnteredRate = NULL, NewSupplementEnteredRate = NULL, EmployeeNumber = NULL, PositionNumberCode = NULL, PositionTypeCode = NULL, PositionTypeDescription = NULL, PositionDistributionsAssignmentTypeCodes = NULL, PositionDistributionsAssignmentTypeDescriptions = NULL, PositionDistributionsBuildingCodes = NULL, PositionDistributionsBuildingDescriptions = NULL, TempSupplementID = NULL, ErrorNumber = NULL, IsMinimumSalarySupplement = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "TempSupplementError", objectId = TempSupplementErrorID, body = list(DataObject = body), searchFields = append("TempSupplementErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List OrganizationChartRelationshipBridges
	#'
	#' This function returns a dataframe or json object of OrganizationChartRelationshipBridges
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OrganizationChartRelationshipBridges. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OrganizationChartRelationshipBridges.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OrganizationChartRelationshipBridge') to get more field paths.
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
	#' @concept Position
	#' @return A list of OrganizationChartRelationshipBridges
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listOrganizationChartRelationshipBridges <- function(searchConditionsList = NULL, OrganizationChartRelationshipBridgeID = F, OrganizationChartID = F, PositionIDSupervisor = F, PositionIDEmployee = F, LevelsBelowSupervisor = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "OrganizationChartRelationshipBridge", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an OrganizationChartRelationshipBridge
	#'
	#' This function returns a dataframe or json object of an OrganizationChartRelationshipBridge
	#' @param OrganizationChartRelationshipBridgeID The ID of the OrganizationChartRelationshipBridge to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OrganizationChartRelationshipBridge. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OrganizationChartRelationshipBridge.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OrganizationChartRelationshipBridge') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of OrganizationChartRelationshipBridge
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getOrganizationChartRelationshipBridge <- function(OrganizationChartRelationshipBridgeID, OrganizationChartID = F, PositionIDSupervisor = F, PositionIDEmployee = F, LevelsBelowSupervisor = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "OrganizationChartRelationshipBridgeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "OrganizationChartRelationshipBridge", objectId = OrganizationChartRelationshipBridgeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an OrganizationChartRelationshipBridge
	#'
	#' This function deletes an OrganizationChartRelationshipBridge
	#' @param OrganizationChartRelationshipBridgeID The ID of the OrganizationChartRelationshipBridge to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The OrganizationChartRelationshipBridgeID of the deleted OrganizationChartRelationshipBridge.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteOrganizationChartRelationshipBridge <- function(OrganizationChartRelationshipBridgeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "OrganizationChartRelationshipBridge", objectId = OrganizationChartRelationshipBridgeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an OrganizationChartRelationshipBridge
	#'
	#' This function creates an OrganizationChartRelationshipBridge
	#' @param fieldNames The field values to give the created OrganizationChartRelationshipBridge. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created OrganizationChartRelationshipBridge
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createOrganizationChartRelationshipBridge <- function(OrganizationChartID = NULL, PositionIDSupervisor = NULL, PositionIDEmployee = NULL, LevelsBelowSupervisor = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "OrganizationChartRelationshipBridge", body = list(DataObject = body), searchFields = append("OrganizationChartRelationshipBridgeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an OrganizationChartRelationshipBridge
	#'
	#' This function modifies an OrganizationChartRelationshipBridge
	#' @param fieldNames The field values to give the modified OrganizationChartRelationshipBridge. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified OrganizationChartRelationshipBridge
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyOrganizationChartRelationshipBridge <- function(OrganizationChartRelationshipBridgeID, OrganizationChartID = NULL, PositionIDSupervisor = NULL, PositionIDEmployee = NULL, LevelsBelowSupervisor = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "OrganizationChartRelationshipBridge", objectId = OrganizationChartRelationshipBridgeID, body = list(DataObject = body), searchFields = append("OrganizationChartRelationshipBridgeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempOrganizationCharts
	#'
	#' This function returns a dataframe or json object of TempOrganizationCharts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempOrganizationCharts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempOrganizationCharts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempOrganizationChart') to get more field paths.
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
	#' @concept Position
	#' @return A list of TempOrganizationCharts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempOrganizationCharts <- function(searchConditionsList = NULL, TempOrganizationChartID = F, Code = F, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempOrganizationChart", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempOrganizationChart
	#'
	#' This function returns a dataframe or json object of a TempOrganizationChart
	#' @param TempOrganizationChartID The ID of the TempOrganizationChart to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempOrganizationChart. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempOrganizationChart.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempOrganizationChart') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of TempOrganizationChart
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempOrganizationChart <- function(TempOrganizationChartID, Code = F, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempOrganizationChartID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "TempOrganizationChart", objectId = TempOrganizationChartID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempOrganizationChart
	#'
	#' This function deletes a TempOrganizationChart
	#' @param TempOrganizationChartID The ID of the TempOrganizationChart to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The TempOrganizationChartID of the deleted TempOrganizationChart.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempOrganizationChart <- function(TempOrganizationChartID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "TempOrganizationChart", objectId = TempOrganizationChartID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempOrganizationChart
	#'
	#' This function creates a TempOrganizationChart
	#' @param fieldNames The field values to give the created TempOrganizationChart. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created TempOrganizationChart
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempOrganizationChart <- function(Code = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "TempOrganizationChart", body = list(DataObject = body), searchFields = append("TempOrganizationChartID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempOrganizationChart
	#'
	#' This function modifies a TempOrganizationChart
	#' @param fieldNames The field values to give the modified TempOrganizationChart. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified TempOrganizationChart
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempOrganizationChart <- function(TempOrganizationChartID, Code = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "TempOrganizationChart", objectId = TempOrganizationChartID, body = list(DataObject = body), searchFields = append("TempOrganizationChartID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempPositionDistributions
	#'
	#' This function returns a dataframe or json object of TempPositionDistributions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempPositionDistributions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempPositionDistributions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempPositionDistribution') to get more field paths.
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
	#' @concept Position
	#' @return A list of TempPositionDistributions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempPositionDistributions <- function(searchConditionsList = NULL, TempPositionDistributionID = F, PositionIDClonedFrom = F, PlanPositionIDClonedFrom = F, AssignmentTypeID = F, BuildingID = F, FTEGroupID = F, DepartmentID = F, BudgetedFTE = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TempPositionID = F, PositionDistributionID = F, PositionDistributionSetID = F, AssignmentTypeCodeDescription = F, NewAssignmentTypeID = F, NewAssignmentTypeCodeDescription = F, BuildingCodeDescription = F, NewBuildingID = F, NewBuildingCodeDescription = F, FTEGroupCodeDescription = F, DepartmentCodeDescription = F, NewDepartmentID = F, NewDepartmentCodeDescription = F, Merged = F, ToDelete = F, Employee = F, PositionType = F, ErrorCount = F, OldAccountDistribution = F, NewAccountDistribution = F, FullPaySecondsPerDay = F, BudgetedHoursPerDay = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempPositionDistribution", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempPositionDistribution
	#'
	#' This function returns a dataframe or json object of a TempPositionDistribution
	#' @param TempPositionDistributionID The ID of the TempPositionDistribution to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempPositionDistribution. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempPositionDistribution.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempPositionDistribution') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of TempPositionDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempPositionDistribution <- function(TempPositionDistributionID, PositionIDClonedFrom = F, PlanPositionIDClonedFrom = F, AssignmentTypeID = F, BuildingID = F, FTEGroupID = F, DepartmentID = F, BudgetedFTE = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TempPositionID = F, PositionDistributionID = F, PositionDistributionSetID = F, AssignmentTypeCodeDescription = F, NewAssignmentTypeID = F, NewAssignmentTypeCodeDescription = F, BuildingCodeDescription = F, NewBuildingID = F, NewBuildingCodeDescription = F, FTEGroupCodeDescription = F, DepartmentCodeDescription = F, NewDepartmentID = F, NewDepartmentCodeDescription = F, Merged = F, ToDelete = F, Employee = F, PositionType = F, ErrorCount = F, OldAccountDistribution = F, NewAccountDistribution = F, FullPaySecondsPerDay = F, BudgetedHoursPerDay = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempPositionDistributionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "TempPositionDistribution", objectId = TempPositionDistributionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempPositionDistribution
	#'
	#' This function deletes a TempPositionDistribution
	#' @param TempPositionDistributionID The ID of the TempPositionDistribution to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The TempPositionDistributionID of the deleted TempPositionDistribution.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempPositionDistribution <- function(TempPositionDistributionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "TempPositionDistribution", objectId = TempPositionDistributionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempPositionDistribution
	#'
	#' This function creates a TempPositionDistribution
	#' @param fieldNames The field values to give the created TempPositionDistribution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created TempPositionDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempPositionDistribution <- function(PositionIDClonedFrom = NULL, PlanPositionIDClonedFrom = NULL, AssignmentTypeID = NULL, BuildingID = NULL, FTEGroupID = NULL, DepartmentID = NULL, BudgetedFTE = NULL, TempPositionID = NULL, PositionDistributionID = NULL, PositionDistributionSetID = NULL, AssignmentTypeCodeDescription = NULL, NewAssignmentTypeID = NULL, NewAssignmentTypeCodeDescription = NULL, BuildingCodeDescription = NULL, NewBuildingID = NULL, NewBuildingCodeDescription = NULL, FTEGroupCodeDescription = NULL, DepartmentCodeDescription = NULL, NewDepartmentID = NULL, NewDepartmentCodeDescription = NULL, Merged = NULL, Employee = NULL, PositionType = NULL, ErrorCount = NULL, OldAccountDistribution = NULL, NewAccountDistribution = NULL, FullPaySecondsPerDay = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "TempPositionDistribution", body = list(DataObject = body), searchFields = append("TempPositionDistributionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempPositionDistribution
	#'
	#' This function modifies a TempPositionDistribution
	#' @param fieldNames The field values to give the modified TempPositionDistribution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified TempPositionDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempPositionDistribution <- function(TempPositionDistributionID, PositionIDClonedFrom = NULL, PlanPositionIDClonedFrom = NULL, AssignmentTypeID = NULL, BuildingID = NULL, FTEGroupID = NULL, DepartmentID = NULL, BudgetedFTE = NULL, TempPositionID = NULL, PositionDistributionID = NULL, PositionDistributionSetID = NULL, AssignmentTypeCodeDescription = NULL, NewAssignmentTypeID = NULL, NewAssignmentTypeCodeDescription = NULL, BuildingCodeDescription = NULL, NewBuildingID = NULL, NewBuildingCodeDescription = NULL, FTEGroupCodeDescription = NULL, DepartmentCodeDescription = NULL, NewDepartmentID = NULL, NewDepartmentCodeDescription = NULL, Merged = NULL, Employee = NULL, PositionType = NULL, ErrorCount = NULL, OldAccountDistribution = NULL, NewAccountDistribution = NULL, FullPaySecondsPerDay = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "TempPositionDistribution", objectId = TempPositionDistributionID, body = list(DataObject = body), searchFields = append("TempPositionDistributionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AssignmentTypePositionDistributionSummaries
	#'
	#' This function returns a dataframe or json object of AssignmentTypePositionDistributionSummaries
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssignmentTypePositionDistributionSummaries. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssignmentTypePositionDistributionSummaries.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssignmentTypePositionDistributionSummary') to get more field paths.
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
	#' @concept Position
	#' @return A list of AssignmentTypePositionDistributionSummaries
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAssignmentTypePositionDistributionSummaries <- function(searchConditionsList = NULL, PositionDistributionIDFirst = F, PositionTypeID = F, AssignmentTypeID = F, BudgetedFTE = F, ClosingAssignmentFTE = F, VacantClosingAssignmentFTE = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "AssignmentTypePositionDistributionSummary", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AssignmentTypePositionDistributionSummary
	#'
	#' This function returns a dataframe or json object of an AssignmentTypePositionDistributionSummary
	#' @param AssignmentTypePositionDistributionSummaryID The ID of the AssignmentTypePositionDistributionSummary to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssignmentTypePositionDistributionSummary. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssignmentTypePositionDistributionSummary.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssignmentTypePositionDistributionSummary') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of AssignmentTypePositionDistributionSummary
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAssignmentTypePositionDistributionSummary <- function(AssignmentTypePositionDistributionSummaryID, PositionDistributionIDFirst = F, PositionTypeID = F, AssignmentTypeID = F, BudgetedFTE = F, ClosingAssignmentFTE = F, VacantClosingAssignmentFTE = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AssignmentTypePositionDistributionSummaryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "AssignmentTypePositionDistributionSummary", objectId = AssignmentTypePositionDistributionSummaryID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AssignmentTypePositionDistributionSummary
	#'
	#' This function deletes an AssignmentTypePositionDistributionSummary
	#' @param AssignmentTypePositionDistributionSummaryID The ID of the AssignmentTypePositionDistributionSummary to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The AssignmentTypePositionDistributionSummaryID of the deleted AssignmentTypePositionDistributionSummary.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAssignmentTypePositionDistributionSummary <- function(AssignmentTypePositionDistributionSummaryID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "AssignmentTypePositionDistributionSummary", objectId = AssignmentTypePositionDistributionSummaryID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AssignmentTypePositionDistributionSummary
	#'
	#' This function creates an AssignmentTypePositionDistributionSummary
	#' @param fieldNames The field values to give the created AssignmentTypePositionDistributionSummary. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created AssignmentTypePositionDistributionSummary
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAssignmentTypePositionDistributionSummary <- function(PositionTypeID = NULL, AssignmentTypeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "AssignmentTypePositionDistributionSummary", body = list(DataObject = body), searchFields = append("AssignmentTypePositionDistributionSummaryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AssignmentTypePositionDistributionSummary
	#'
	#' This function modifies an AssignmentTypePositionDistributionSummary
	#' @param fieldNames The field values to give the modified AssignmentTypePositionDistributionSummary. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified AssignmentTypePositionDistributionSummary
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAssignmentTypePositionDistributionSummary <- function(AssignmentTypePositionDistributionSummaryID, PositionTypeID = NULL, AssignmentTypeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "AssignmentTypePositionDistributionSummary", objectId = AssignmentTypePositionDistributionSummaryID, body = list(DataObject = body), searchFields = append("AssignmentTypePositionDistributionSummaryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List BuildingPositionDistributionSummaries
	#'
	#' This function returns a dataframe or json object of BuildingPositionDistributionSummaries
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BuildingPositionDistributionSummaries. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BuildingPositionDistributionSummaries.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BuildingPositionDistributionSummary') to get more field paths.
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
	#' @concept Position
	#' @return A list of BuildingPositionDistributionSummaries
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listBuildingPositionDistributionSummaries <- function(searchConditionsList = NULL, PositionDistributionIDFirst = F, PositionTypeID = F, BuildingID = F, BudgetedFTE = F, ClosingAssignmentFTE = F, VacantClosingAssignmentFTE = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "BuildingPositionDistributionSummary", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a BuildingPositionDistributionSummary
	#'
	#' This function returns a dataframe or json object of a BuildingPositionDistributionSummary
	#' @param BuildingPositionDistributionSummaryID The ID of the BuildingPositionDistributionSummary to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BuildingPositionDistributionSummary. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BuildingPositionDistributionSummary.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BuildingPositionDistributionSummary') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of BuildingPositionDistributionSummary
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getBuildingPositionDistributionSummary <- function(BuildingPositionDistributionSummaryID, PositionDistributionIDFirst = F, PositionTypeID = F, BuildingID = F, BudgetedFTE = F, ClosingAssignmentFTE = F, VacantClosingAssignmentFTE = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "BuildingPositionDistributionSummaryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "BuildingPositionDistributionSummary", objectId = BuildingPositionDistributionSummaryID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a BuildingPositionDistributionSummary
	#'
	#' This function deletes a BuildingPositionDistributionSummary
	#' @param BuildingPositionDistributionSummaryID The ID of the BuildingPositionDistributionSummary to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The BuildingPositionDistributionSummaryID of the deleted BuildingPositionDistributionSummary.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteBuildingPositionDistributionSummary <- function(BuildingPositionDistributionSummaryID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "BuildingPositionDistributionSummary", objectId = BuildingPositionDistributionSummaryID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a BuildingPositionDistributionSummary
	#'
	#' This function creates a BuildingPositionDistributionSummary
	#' @param fieldNames The field values to give the created BuildingPositionDistributionSummary. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created BuildingPositionDistributionSummary
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createBuildingPositionDistributionSummary <- function(PositionTypeID = NULL, BuildingID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "BuildingPositionDistributionSummary", body = list(DataObject = body), searchFields = append("BuildingPositionDistributionSummaryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a BuildingPositionDistributionSummary
	#'
	#' This function modifies a BuildingPositionDistributionSummary
	#' @param fieldNames The field values to give the modified BuildingPositionDistributionSummary. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified BuildingPositionDistributionSummary
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyBuildingPositionDistributionSummary <- function(BuildingPositionDistributionSummaryID, PositionTypeID = NULL, BuildingID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "BuildingPositionDistributionSummary", objectId = BuildingPositionDistributionSummaryID, body = list(DataObject = body), searchFields = append("BuildingPositionDistributionSummaryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PositionDistributions
	#'
	#' This function returns a dataframe or json object of PositionDistributions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PositionDistributions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PositionDistributions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PositionDistribution') to get more field paths.
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
	#' @concept Position
	#' @return A list of PositionDistributions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPositionDistributions <- function(searchConditionsList = NULL, PositionDistributionID = F, AssignmentTypeID = F, PositionID = F, BuildingID = F, FTEGroupID = F, DepartmentID = F, BudgetedFTE = F, ClosingAssignmentFTE = F, VacantClosingAssignmentFTE = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PositionDistributionSetID = F, BudgetedHoursPerDay = F, CalculatedBudgetedHoursPerDay = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "PositionDistribution", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PositionDistribution
	#'
	#' This function returns a dataframe or json object of a PositionDistribution
	#' @param PositionDistributionID The ID of the PositionDistribution to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PositionDistribution. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PositionDistribution.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PositionDistribution') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of PositionDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPositionDistribution <- function(PositionDistributionID, AssignmentTypeID = F, PositionID = F, BuildingID = F, FTEGroupID = F, DepartmentID = F, BudgetedFTE = F, ClosingAssignmentFTE = F, VacantClosingAssignmentFTE = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PositionDistributionSetID = F, BudgetedHoursPerDay = F, CalculatedBudgetedHoursPerDay = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PositionDistributionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "PositionDistribution", objectId = PositionDistributionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PositionDistribution
	#'
	#' This function deletes a PositionDistribution
	#' @param PositionDistributionID The ID of the PositionDistribution to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The PositionDistributionID of the deleted PositionDistribution.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePositionDistribution <- function(PositionDistributionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "PositionDistribution", objectId = PositionDistributionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PositionDistribution
	#'
	#' This function creates a PositionDistribution
	#' @param fieldNames The field values to give the created PositionDistribution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created PositionDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPositionDistribution <- function(AssignmentTypeID = NULL, BuildingID = NULL, FTEGroupID = NULL, DepartmentID = NULL, BudgetedFTE = NULL, PositionDistributionSetID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "PositionDistribution", body = list(DataObject = body), searchFields = append("PositionDistributionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PositionDistribution
	#'
	#' This function modifies a PositionDistribution
	#' @param fieldNames The field values to give the modified PositionDistribution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified PositionDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPositionDistribution <- function(PositionDistributionID, AssignmentTypeID = NULL, BuildingID = NULL, FTEGroupID = NULL, DepartmentID = NULL, BudgetedFTE = NULL, PositionDistributionSetID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "PositionDistribution", objectId = PositionDistributionID, body = list(DataObject = body), searchFields = append("PositionDistributionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EmployeePositionTypes
	#'
	#' This function returns a dataframe or json object of EmployeePositionTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeePositionTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeePositionTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeePositionType') to get more field paths.
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
	#' @concept Position
	#' @return A list of EmployeePositionTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEmployeePositionTypes <- function(searchConditionsList = NULL, EmployeePositionTypeID = F, EmployeeID = F, PositionTypeID = F, CurrentTotalFTE = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalPay = F, HasPositionsToMerge = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "EmployeePositionType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EmployeePositionType
	#'
	#' This function returns a dataframe or json object of an EmployeePositionType
	#' @param EmployeePositionTypeID The ID of the EmployeePositionType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeePositionType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeePositionType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeePositionType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of EmployeePositionType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEmployeePositionType <- function(EmployeePositionTypeID, EmployeeID = F, PositionTypeID = F, CurrentTotalFTE = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalPay = F, HasPositionsToMerge = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EmployeePositionTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "EmployeePositionType", objectId = EmployeePositionTypeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EmployeePositionType
	#'
	#' This function deletes an EmployeePositionType
	#' @param EmployeePositionTypeID The ID of the EmployeePositionType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The EmployeePositionTypeID of the deleted EmployeePositionType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEmployeePositionType <- function(EmployeePositionTypeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "EmployeePositionType", objectId = EmployeePositionTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EmployeePositionType
	#'
	#' This function creates an EmployeePositionType
	#' @param fieldNames The field values to give the created EmployeePositionType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created EmployeePositionType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEmployeePositionType <- function(EmployeeID = NULL, PositionTypeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "EmployeePositionType", body = list(DataObject = body), searchFields = append("EmployeePositionTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EmployeePositionType
	#'
	#' This function modifies an EmployeePositionType
	#' @param fieldNames The field values to give the modified EmployeePositionType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified EmployeePositionType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEmployeePositionType <- function(EmployeePositionTypeID, EmployeeID = NULL, PositionTypeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "EmployeePositionType", objectId = EmployeePositionTypeID, body = list(DataObject = body), searchFields = append("EmployeePositionTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AssignmentDelimitedFileFormats
	#'
	#' This function returns a dataframe or json object of AssignmentDelimitedFileFormats
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssignmentDelimitedFileFormats. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssignmentDelimitedFileFormats.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssignmentDelimitedFileFormat') to get more field paths.
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
	#' @concept Position
	#' @return A list of AssignmentDelimitedFileFormats
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAssignmentDelimitedFileFormats <- function(searchConditionsList = NULL, AssignmentDelimitedFileFormatID = F, AssignmentThirdPartyFormatID = F, NumberOfHeaderRows = F, DelimiterCharacter = F, EmployeeIdentifierColumnNumber = F, PositionNumberColumnNumber = F, PositionStartDateColumnNumber = F, PositionEndDateColumnNumber = F, PositionCalendarColumnNumber = F, FullHoursPerDayColumnNumber = F, PositionGroupColumnNumber = F, JobTypeColumnNumber = F, PositionTypeColumnNumber = F, AssignmentTypeColumnNumber = F, BuildingColumnNumber = F, FTEGroupColumnNumber = F, DepartmentColumnNumber = F, PositionFTEColumnNumber = F, PositionDistributionAccountDistributionColumnNumber = F, AssignmentStartDateColumnNumber = F, AssignmentEndDateColumnNumber = F, AssignmentHoursPerDayColumnNumber = F, AssignmentFTEColumnNumber = F, SalaryCalculationMethodColumnNumber = F, MatrixColumnNumber = F, StepColumnNumber = F, LaneColumnNumber = F, CreditsColumnNumber = F, PlacementColumnNumber = F, RateColumnNumber = F, EntitlementColumnNumber = F, AssignmentTimeTrackingGroupColumnNumber = F, NextYearIntentColumnNumber = F, SupplementType1ColumnNumber = F, SupplementAmount1ColumnNumber = F, SupplementType2ColumnNumber = F, SupplementAmount2ColumnNumber = F, SupplementType3ColumnNumber = F, SupplementAmount3ColumnNumber = F, DelimiterType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "AssignmentDelimitedFileFormat", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AssignmentDelimitedFileFormat
	#'
	#' This function returns a dataframe or json object of an AssignmentDelimitedFileFormat
	#' @param AssignmentDelimitedFileFormatID The ID of the AssignmentDelimitedFileFormat to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssignmentDelimitedFileFormat. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssignmentDelimitedFileFormat.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssignmentDelimitedFileFormat') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of AssignmentDelimitedFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAssignmentDelimitedFileFormat <- function(AssignmentDelimitedFileFormatID, AssignmentThirdPartyFormatID = F, NumberOfHeaderRows = F, DelimiterCharacter = F, EmployeeIdentifierColumnNumber = F, PositionNumberColumnNumber = F, PositionStartDateColumnNumber = F, PositionEndDateColumnNumber = F, PositionCalendarColumnNumber = F, FullHoursPerDayColumnNumber = F, PositionGroupColumnNumber = F, JobTypeColumnNumber = F, PositionTypeColumnNumber = F, AssignmentTypeColumnNumber = F, BuildingColumnNumber = F, FTEGroupColumnNumber = F, DepartmentColumnNumber = F, PositionFTEColumnNumber = F, PositionDistributionAccountDistributionColumnNumber = F, AssignmentStartDateColumnNumber = F, AssignmentEndDateColumnNumber = F, AssignmentHoursPerDayColumnNumber = F, AssignmentFTEColumnNumber = F, SalaryCalculationMethodColumnNumber = F, MatrixColumnNumber = F, StepColumnNumber = F, LaneColumnNumber = F, CreditsColumnNumber = F, PlacementColumnNumber = F, RateColumnNumber = F, EntitlementColumnNumber = F, AssignmentTimeTrackingGroupColumnNumber = F, NextYearIntentColumnNumber = F, SupplementType1ColumnNumber = F, SupplementAmount1ColumnNumber = F, SupplementType2ColumnNumber = F, SupplementAmount2ColumnNumber = F, SupplementType3ColumnNumber = F, SupplementAmount3ColumnNumber = F, DelimiterType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AssignmentDelimitedFileFormatID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "AssignmentDelimitedFileFormat", objectId = AssignmentDelimitedFileFormatID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AssignmentDelimitedFileFormat
	#'
	#' This function deletes an AssignmentDelimitedFileFormat
	#' @param AssignmentDelimitedFileFormatID The ID of the AssignmentDelimitedFileFormat to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The AssignmentDelimitedFileFormatID of the deleted AssignmentDelimitedFileFormat.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAssignmentDelimitedFileFormat <- function(AssignmentDelimitedFileFormatID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "AssignmentDelimitedFileFormat", objectId = AssignmentDelimitedFileFormatID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AssignmentDelimitedFileFormat
	#'
	#' This function creates an AssignmentDelimitedFileFormat
	#' @param fieldNames The field values to give the created AssignmentDelimitedFileFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created AssignmentDelimitedFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAssignmentDelimitedFileFormat <- function(AssignmentThirdPartyFormatID = NULL, NumberOfHeaderRows = NULL, DelimiterCharacter = NULL, EmployeeIdentifierColumnNumber = NULL, PositionNumberColumnNumber = NULL, PositionStartDateColumnNumber = NULL, PositionEndDateColumnNumber = NULL, PositionCalendarColumnNumber = NULL, FullHoursPerDayColumnNumber = NULL, PositionGroupColumnNumber = NULL, JobTypeColumnNumber = NULL, PositionTypeColumnNumber = NULL, AssignmentTypeColumnNumber = NULL, BuildingColumnNumber = NULL, FTEGroupColumnNumber = NULL, DepartmentColumnNumber = NULL, PositionFTEColumnNumber = NULL, PositionDistributionAccountDistributionColumnNumber = NULL, AssignmentStartDateColumnNumber = NULL, AssignmentEndDateColumnNumber = NULL, AssignmentHoursPerDayColumnNumber = NULL, AssignmentFTEColumnNumber = NULL, SalaryCalculationMethodColumnNumber = NULL, MatrixColumnNumber = NULL, StepColumnNumber = NULL, LaneColumnNumber = NULL, CreditsColumnNumber = NULL, PlacementColumnNumber = NULL, RateColumnNumber = NULL, EntitlementColumnNumber = NULL, AssignmentTimeTrackingGroupColumnNumber = NULL, NextYearIntentColumnNumber = NULL, SupplementType1ColumnNumber = NULL, SupplementAmount1ColumnNumber = NULL, SupplementType2ColumnNumber = NULL, SupplementAmount2ColumnNumber = NULL, SupplementType3ColumnNumber = NULL, SupplementAmount3ColumnNumber = NULL, DelimiterType = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "AssignmentDelimitedFileFormat", body = list(DataObject = body), searchFields = append("AssignmentDelimitedFileFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AssignmentDelimitedFileFormat
	#'
	#' This function modifies an AssignmentDelimitedFileFormat
	#' @param fieldNames The field values to give the modified AssignmentDelimitedFileFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified AssignmentDelimitedFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAssignmentDelimitedFileFormat <- function(AssignmentDelimitedFileFormatID, AssignmentThirdPartyFormatID = NULL, NumberOfHeaderRows = NULL, DelimiterCharacter = NULL, EmployeeIdentifierColumnNumber = NULL, PositionNumberColumnNumber = NULL, PositionStartDateColumnNumber = NULL, PositionEndDateColumnNumber = NULL, PositionCalendarColumnNumber = NULL, FullHoursPerDayColumnNumber = NULL, PositionGroupColumnNumber = NULL, JobTypeColumnNumber = NULL, PositionTypeColumnNumber = NULL, AssignmentTypeColumnNumber = NULL, BuildingColumnNumber = NULL, FTEGroupColumnNumber = NULL, DepartmentColumnNumber = NULL, PositionFTEColumnNumber = NULL, PositionDistributionAccountDistributionColumnNumber = NULL, AssignmentStartDateColumnNumber = NULL, AssignmentEndDateColumnNumber = NULL, AssignmentHoursPerDayColumnNumber = NULL, AssignmentFTEColumnNumber = NULL, SalaryCalculationMethodColumnNumber = NULL, MatrixColumnNumber = NULL, StepColumnNumber = NULL, LaneColumnNumber = NULL, CreditsColumnNumber = NULL, PlacementColumnNumber = NULL, RateColumnNumber = NULL, EntitlementColumnNumber = NULL, AssignmentTimeTrackingGroupColumnNumber = NULL, NextYearIntentColumnNumber = NULL, SupplementType1ColumnNumber = NULL, SupplementAmount1ColumnNumber = NULL, SupplementType2ColumnNumber = NULL, SupplementAmount2ColumnNumber = NULL, SupplementType3ColumnNumber = NULL, SupplementAmount3ColumnNumber = NULL, DelimiterType = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "AssignmentDelimitedFileFormat", objectId = AssignmentDelimitedFileFormatID, body = list(DataObject = body), searchFields = append("AssignmentDelimitedFileFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AssignmentThirdPartyFormats
	#'
	#' This function returns a dataframe or json object of AssignmentThirdPartyFormats
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssignmentThirdPartyFormats. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssignmentThirdPartyFormats.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssignmentThirdPartyFormat') to get more field paths.
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
	#' @concept Position
	#' @return A list of AssignmentThirdPartyFormats
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAssignmentThirdPartyFormats <- function(searchConditionsList = NULL, AssignmentThirdPartyFormatID = F, DistrictID = F, Code = F, Description = F, EmployeeIdentification = F, DateFormat = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "AssignmentThirdPartyFormat", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AssignmentThirdPartyFormat
	#'
	#' This function returns a dataframe or json object of an AssignmentThirdPartyFormat
	#' @param AssignmentThirdPartyFormatID The ID of the AssignmentThirdPartyFormat to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssignmentThirdPartyFormat. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssignmentThirdPartyFormat.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssignmentThirdPartyFormat') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of AssignmentThirdPartyFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAssignmentThirdPartyFormat <- function(AssignmentThirdPartyFormatID, DistrictID = F, Code = F, Description = F, EmployeeIdentification = F, DateFormat = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AssignmentThirdPartyFormatID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "AssignmentThirdPartyFormat", objectId = AssignmentThirdPartyFormatID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AssignmentThirdPartyFormat
	#'
	#' This function deletes an AssignmentThirdPartyFormat
	#' @param AssignmentThirdPartyFormatID The ID of the AssignmentThirdPartyFormat to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The AssignmentThirdPartyFormatID of the deleted AssignmentThirdPartyFormat.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAssignmentThirdPartyFormat <- function(AssignmentThirdPartyFormatID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "AssignmentThirdPartyFormat", objectId = AssignmentThirdPartyFormatID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AssignmentThirdPartyFormat
	#'
	#' This function creates an AssignmentThirdPartyFormat
	#' @param fieldNames The field values to give the created AssignmentThirdPartyFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created AssignmentThirdPartyFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAssignmentThirdPartyFormat <- function(DistrictID = NULL, Code = NULL, Description = NULL, EmployeeIdentification = NULL, DateFormat = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "AssignmentThirdPartyFormat", body = list(DataObject = body), searchFields = append("AssignmentThirdPartyFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AssignmentThirdPartyFormat
	#'
	#' This function modifies an AssignmentThirdPartyFormat
	#' @param fieldNames The field values to give the modified AssignmentThirdPartyFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified AssignmentThirdPartyFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAssignmentThirdPartyFormat <- function(AssignmentThirdPartyFormatID, DistrictID = NULL, Code = NULL, Description = NULL, EmployeeIdentification = NULL, DateFormat = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "AssignmentThirdPartyFormat", objectId = AssignmentThirdPartyFormatID, body = list(DataObject = body), searchFields = append("AssignmentThirdPartyFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AssignmentThirdPartyImports
	#'
	#' This function returns a dataframe or json object of AssignmentThirdPartyImports
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssignmentThirdPartyImports. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssignmentThirdPartyImports.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssignmentThirdPartyImport') to get more field paths.
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
	#' @concept Position
	#' @return A list of AssignmentThirdPartyImports
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAssignmentThirdPartyImports <- function(searchConditionsList = NULL, AssignmentThirdPartyImportID = F, AssignmentThirdPartyFormatID = F, ImportTime = F, MediaID = F, MediaIDFailedResult = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "AssignmentThirdPartyImport", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AssignmentThirdPartyImport
	#'
	#' This function returns a dataframe or json object of an AssignmentThirdPartyImport
	#' @param AssignmentThirdPartyImportID The ID of the AssignmentThirdPartyImport to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssignmentThirdPartyImport. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssignmentThirdPartyImport.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssignmentThirdPartyImport') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of AssignmentThirdPartyImport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAssignmentThirdPartyImport <- function(AssignmentThirdPartyImportID, AssignmentThirdPartyFormatID = F, ImportTime = F, MediaID = F, MediaIDFailedResult = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AssignmentThirdPartyImportID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "AssignmentThirdPartyImport", objectId = AssignmentThirdPartyImportID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AssignmentThirdPartyImport
	#'
	#' This function deletes an AssignmentThirdPartyImport
	#' @param AssignmentThirdPartyImportID The ID of the AssignmentThirdPartyImport to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The AssignmentThirdPartyImportID of the deleted AssignmentThirdPartyImport.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAssignmentThirdPartyImport <- function(AssignmentThirdPartyImportID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "AssignmentThirdPartyImport", objectId = AssignmentThirdPartyImportID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AssignmentThirdPartyImport
	#'
	#' This function creates an AssignmentThirdPartyImport
	#' @param fieldNames The field values to give the created AssignmentThirdPartyImport. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created AssignmentThirdPartyImport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAssignmentThirdPartyImport <- function(AssignmentThirdPartyFormatID = NULL, ImportTime = NULL, MediaID = NULL, MediaIDFailedResult = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "AssignmentThirdPartyImport", body = list(DataObject = body), searchFields = append("AssignmentThirdPartyImportID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AssignmentThirdPartyImport
	#'
	#' This function modifies an AssignmentThirdPartyImport
	#' @param fieldNames The field values to give the modified AssignmentThirdPartyImport. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified AssignmentThirdPartyImport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAssignmentThirdPartyImport <- function(AssignmentThirdPartyImportID, AssignmentThirdPartyFormatID = NULL, ImportTime = NULL, MediaID = NULL, MediaIDFailedResult = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "AssignmentThirdPartyImport", objectId = AssignmentThirdPartyImportID, body = list(DataObject = body), searchFields = append("AssignmentThirdPartyImportID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PositionDistributionSets
	#'
	#' This function returns a dataframe or json object of PositionDistributionSets
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PositionDistributionSets. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PositionDistributionSets.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PositionDistributionSet') to get more field paths.
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
	#' @concept Position
	#' @return A list of PositionDistributionSets
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPositionDistributionSets <- function(searchConditionsList = NULL, PositionDistributionSetID = F, PositionID = F, StartDate = F, EndDate = F, BudgetedFTE = F, IsCurrent = F, IsClosing = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, PositionDistributionsDepartmentCodes = F, PositionDistributionsDepartmentDescriptions = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsFTEGroupCodes = F, PositionDistributionsFTEGroupDescriptions = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AccountDistribution = F, BudgetedHoursPerDay = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "PositionDistributionSet", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PositionDistributionSet
	#'
	#' This function returns a dataframe or json object of a PositionDistributionSet
	#' @param PositionDistributionSetID The ID of the PositionDistributionSet to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PositionDistributionSet. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PositionDistributionSet.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PositionDistributionSet') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of PositionDistributionSet
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPositionDistributionSet <- function(PositionDistributionSetID, PositionID = F, StartDate = F, EndDate = F, BudgetedFTE = F, IsCurrent = F, IsClosing = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, PositionDistributionsDepartmentCodes = F, PositionDistributionsDepartmentDescriptions = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsFTEGroupCodes = F, PositionDistributionsFTEGroupDescriptions = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AccountDistribution = F, BudgetedHoursPerDay = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PositionDistributionSetID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "PositionDistributionSet", objectId = PositionDistributionSetID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PositionDistributionSet
	#'
	#' This function deletes a PositionDistributionSet
	#' @param PositionDistributionSetID The ID of the PositionDistributionSet to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The PositionDistributionSetID of the deleted PositionDistributionSet.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePositionDistributionSet <- function(PositionDistributionSetID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "PositionDistributionSet", objectId = PositionDistributionSetID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PositionDistributionSet
	#'
	#' This function creates a PositionDistributionSet
	#' @param fieldNames The field values to give the created PositionDistributionSet. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created PositionDistributionSet
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPositionDistributionSet <- function(PositionID = NULL, StartDate = NULL, EndDate = NULL, BudgetedFTE = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "PositionDistributionSet", body = list(DataObject = body), searchFields = append("PositionDistributionSetID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PositionDistributionSet
	#'
	#' This function modifies a PositionDistributionSet
	#' @param fieldNames The field values to give the modified PositionDistributionSet. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified PositionDistributionSet
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPositionDistributionSet <- function(PositionDistributionSetID, PositionID = NULL, StartDate = NULL, EndDate = NULL, BudgetedFTE = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "PositionDistributionSet", objectId = PositionDistributionSetID, body = list(DataObject = body), searchFields = append("PositionDistributionSetID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PositionDistributionAccountDistributions
	#'
	#' This function returns a dataframe or json object of PositionDistributionAccountDistributions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PositionDistributionAccountDistributions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PositionDistributionAccountDistributions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PositionDistributionAccountDistribution') to get more field paths.
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
	#' @concept Position
	#' @return A list of PositionDistributionAccountDistributions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPositionDistributionAccountDistributions <- function(searchConditionsList = NULL, PositionDistributionAccountDistributionID = F, PositionDistributionID = F, AccountID = F, DistributionPercent = F, ProratedDistributionPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "PositionDistributionAccountDistribution", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PositionDistributionAccountDistribution
	#'
	#' This function returns a dataframe or json object of a PositionDistributionAccountDistribution
	#' @param PositionDistributionAccountDistributionID The ID of the PositionDistributionAccountDistribution to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PositionDistributionAccountDistribution. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PositionDistributionAccountDistribution.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PositionDistributionAccountDistribution') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of PositionDistributionAccountDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPositionDistributionAccountDistribution <- function(PositionDistributionAccountDistributionID, PositionDistributionID = F, AccountID = F, DistributionPercent = F, ProratedDistributionPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PositionDistributionAccountDistributionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "PositionDistributionAccountDistribution", objectId = PositionDistributionAccountDistributionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PositionDistributionAccountDistribution
	#'
	#' This function deletes a PositionDistributionAccountDistribution
	#' @param PositionDistributionAccountDistributionID The ID of the PositionDistributionAccountDistribution to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The PositionDistributionAccountDistributionID of the deleted PositionDistributionAccountDistribution.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePositionDistributionAccountDistribution <- function(PositionDistributionAccountDistributionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "PositionDistributionAccountDistribution", objectId = PositionDistributionAccountDistributionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PositionDistributionAccountDistribution
	#'
	#' This function creates a PositionDistributionAccountDistribution
	#' @param fieldNames The field values to give the created PositionDistributionAccountDistribution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created PositionDistributionAccountDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPositionDistributionAccountDistribution <- function(PositionDistributionID = NULL, AccountID = NULL, DistributionPercent = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "PositionDistributionAccountDistribution", body = list(DataObject = body), searchFields = append("PositionDistributionAccountDistributionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PositionDistributionAccountDistribution
	#'
	#' This function modifies a PositionDistributionAccountDistribution
	#' @param fieldNames The field values to give the modified PositionDistributionAccountDistribution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified PositionDistributionAccountDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPositionDistributionAccountDistribution <- function(PositionDistributionAccountDistributionID, PositionDistributionID = NULL, AccountID = NULL, DistributionPercent = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "PositionDistributionAccountDistribution", objectId = PositionDistributionAccountDistributionID, body = list(DataObject = body), searchFields = append("PositionDistributionAccountDistributionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempPositionDistributionAccountDistributions
	#'
	#' This function returns a dataframe or json object of TempPositionDistributionAccountDistributions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempPositionDistributionAccountDistributions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempPositionDistributionAccountDistributions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempPositionDistributionAccountDistribution') to get more field paths.
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
	#' @concept Position
	#' @return A list of TempPositionDistributionAccountDistributions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempPositionDistributionAccountDistributions <- function(searchConditionsList = NULL, TempPositionDistributionAccountDistributionID = F, PositionDistributionID = F, TempPositionDistributionID = F, AccountID = F, DistributionPercent = F, StateConcordDepartmentTNID = F, OldAccount = F, NewAccount = F, TempAssignmentPayTypeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempPositionDistributionAccountDistribution", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempPositionDistributionAccountDistribution
	#'
	#' This function returns a dataframe or json object of a TempPositionDistributionAccountDistribution
	#' @param TempPositionDistributionAccountDistributionID The ID of the TempPositionDistributionAccountDistribution to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempPositionDistributionAccountDistribution. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempPositionDistributionAccountDistribution.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempPositionDistributionAccountDistribution') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of TempPositionDistributionAccountDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempPositionDistributionAccountDistribution <- function(TempPositionDistributionAccountDistributionID, PositionDistributionID = F, TempPositionDistributionID = F, AccountID = F, DistributionPercent = F, StateConcordDepartmentTNID = F, OldAccount = F, NewAccount = F, TempAssignmentPayTypeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempPositionDistributionAccountDistributionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "TempPositionDistributionAccountDistribution", objectId = TempPositionDistributionAccountDistributionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempPositionDistributionAccountDistribution
	#'
	#' This function deletes a TempPositionDistributionAccountDistribution
	#' @param TempPositionDistributionAccountDistributionID The ID of the TempPositionDistributionAccountDistribution to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The TempPositionDistributionAccountDistributionID of the deleted TempPositionDistributionAccountDistribution.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempPositionDistributionAccountDistribution <- function(TempPositionDistributionAccountDistributionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "TempPositionDistributionAccountDistribution", objectId = TempPositionDistributionAccountDistributionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempPositionDistributionAccountDistribution
	#'
	#' This function creates a TempPositionDistributionAccountDistribution
	#' @param fieldNames The field values to give the created TempPositionDistributionAccountDistribution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created TempPositionDistributionAccountDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempPositionDistributionAccountDistribution <- function(PositionDistributionID = NULL, TempPositionDistributionID = NULL, AccountID = NULL, DistributionPercent = NULL, StateConcordDepartmentTNID = NULL, OldAccount = NULL, NewAccount = NULL, TempAssignmentPayTypeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "TempPositionDistributionAccountDistribution", body = list(DataObject = body), searchFields = append("TempPositionDistributionAccountDistributionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempPositionDistributionAccountDistribution
	#'
	#' This function modifies a TempPositionDistributionAccountDistribution
	#' @param fieldNames The field values to give the modified TempPositionDistributionAccountDistribution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified TempPositionDistributionAccountDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempPositionDistributionAccountDistribution <- function(TempPositionDistributionAccountDistributionID, PositionDistributionID = NULL, TempPositionDistributionID = NULL, AccountID = NULL, DistributionPercent = NULL, StateConcordDepartmentTNID = NULL, OldAccount = NULL, NewAccount = NULL, TempAssignmentPayTypeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "TempPositionDistributionAccountDistribution", objectId = TempPositionDistributionAccountDistributionID, body = list(DataObject = body), searchFields = append("TempPositionDistributionAccountDistributionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempAssignmentCalendarDays
	#'
	#' This function returns a dataframe or json object of TempAssignmentCalendarDays
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAssignmentCalendarDays. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAssignmentCalendarDays.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAssignmentCalendarDay') to get more field paths.
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
	#' @concept Position
	#' @return A list of TempAssignmentCalendarDays
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempAssignmentCalendarDays <- function(searchConditionsList = NULL, TempAssignmentCalendarDayID = F, Date = F, OldIsWorkday = F, IsWorkday = F, OldIsPaid = F, IsPaid = F, OldSeconds = F, Seconds = F, ErrorCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempAssignmentCalendarDay", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempAssignmentCalendarDay
	#'
	#' This function returns a dataframe or json object of a TempAssignmentCalendarDay
	#' @param TempAssignmentCalendarDayID The ID of the TempAssignmentCalendarDay to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAssignmentCalendarDay. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAssignmentCalendarDay.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAssignmentCalendarDay') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of TempAssignmentCalendarDay
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempAssignmentCalendarDay <- function(TempAssignmentCalendarDayID, Date = F, OldIsWorkday = F, IsWorkday = F, OldIsPaid = F, IsPaid = F, OldSeconds = F, Seconds = F, ErrorCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempAssignmentCalendarDayID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "TempAssignmentCalendarDay", objectId = TempAssignmentCalendarDayID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempAssignmentCalendarDay
	#'
	#' This function deletes a TempAssignmentCalendarDay
	#' @param TempAssignmentCalendarDayID The ID of the TempAssignmentCalendarDay to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The TempAssignmentCalendarDayID of the deleted TempAssignmentCalendarDay.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempAssignmentCalendarDay <- function(TempAssignmentCalendarDayID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "TempAssignmentCalendarDay", objectId = TempAssignmentCalendarDayID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempAssignmentCalendarDay
	#'
	#' This function creates a TempAssignmentCalendarDay
	#' @param fieldNames The field values to give the created TempAssignmentCalendarDay. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created TempAssignmentCalendarDay
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempAssignmentCalendarDay <- function(Date = NULL, OldIsWorkday = NULL, IsWorkday = NULL, OldIsPaid = NULL, IsPaid = NULL, OldSeconds = NULL, Seconds = NULL, ErrorCount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "TempAssignmentCalendarDay", body = list(DataObject = body), searchFields = append("TempAssignmentCalendarDayID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempAssignmentCalendarDay
	#'
	#' This function modifies a TempAssignmentCalendarDay
	#' @param fieldNames The field values to give the modified TempAssignmentCalendarDay. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified TempAssignmentCalendarDay
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempAssignmentCalendarDay <- function(TempAssignmentCalendarDayID, Date = NULL, OldIsWorkday = NULL, IsWorkday = NULL, OldIsPaid = NULL, IsPaid = NULL, OldSeconds = NULL, Seconds = NULL, ErrorCount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "TempAssignmentCalendarDay", objectId = TempAssignmentCalendarDayID, body = list(DataObject = body), searchFields = append("TempAssignmentCalendarDayID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AssignmentCalendarDays
	#'
	#' This function returns a dataframe or json object of AssignmentCalendarDays
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssignmentCalendarDays. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssignmentCalendarDays.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssignmentCalendarDay') to get more field paths.
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
	#' @concept Position
	#' @return A list of AssignmentCalendarDays
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAssignmentCalendarDays <- function(searchConditionsList = NULL, CalendarID = F, CalendarDayID = F, AssignmentID = F, EmployeeID = F, AssignmentDetailID = F, AssignmentCalendarDayOverrideID = F, Date = F, IsPaid = F, IsWorkday = F, Comment = F, AssignmentDetailSecondsPerDay = F, PaidSeconds = F, OverrideExists = F, IsPaidHoliday = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "AssignmentCalendarDay", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AssignmentCalendarDay
	#'
	#' This function returns a dataframe or json object of an AssignmentCalendarDay
	#' @param AssignmentCalendarDayID The ID of the AssignmentCalendarDay to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssignmentCalendarDay. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssignmentCalendarDay.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssignmentCalendarDay') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of AssignmentCalendarDay
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAssignmentCalendarDay <- function(AssignmentCalendarDayID, CalendarID = F, CalendarDayID = F, AssignmentID = F, EmployeeID = F, AssignmentDetailID = F, AssignmentCalendarDayOverrideID = F, Date = F, IsPaid = F, IsWorkday = F, Comment = F, AssignmentDetailSecondsPerDay = F, PaidSeconds = F, OverrideExists = F, IsPaidHoliday = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AssignmentCalendarDayID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "AssignmentCalendarDay", objectId = AssignmentCalendarDayID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AssignmentCalendarDay
	#'
	#' This function deletes an AssignmentCalendarDay
	#' @param AssignmentCalendarDayID The ID of the AssignmentCalendarDay to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The AssignmentCalendarDayID of the deleted AssignmentCalendarDay.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAssignmentCalendarDay <- function(AssignmentCalendarDayID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "AssignmentCalendarDay", objectId = AssignmentCalendarDayID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AssignmentCalendarDay
	#'
	#' This function creates an AssignmentCalendarDay
	#' @param fieldNames The field values to give the created AssignmentCalendarDay. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created AssignmentCalendarDay
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAssignmentCalendarDay <- function(CalendarID = NULL, CalendarDayID = NULL, EmployeeID = NULL, AssignmentDetailID = NULL, AssignmentCalendarDayOverrideID = NULL, Date = NULL, IsPaid = NULL, IsWorkday = NULL, Comment = NULL, AssignmentDetailSecondsPerDay = NULL, PaidSeconds = NULL, OverrideExists = NULL, IsPaidHoliday = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "AssignmentCalendarDay", body = list(DataObject = body), searchFields = append("AssignmentCalendarDayID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AssignmentCalendarDay
	#'
	#' This function modifies an AssignmentCalendarDay
	#' @param fieldNames The field values to give the modified AssignmentCalendarDay. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified AssignmentCalendarDay
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAssignmentCalendarDay <- function(AssignmentCalendarDayID, CalendarID = NULL, CalendarDayID = NULL, EmployeeID = NULL, AssignmentDetailID = NULL, AssignmentCalendarDayOverrideID = NULL, Date = NULL, IsPaid = NULL, IsWorkday = NULL, Comment = NULL, AssignmentDetailSecondsPerDay = NULL, PaidSeconds = NULL, OverrideExists = NULL, IsPaidHoliday = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "AssignmentCalendarDay", objectId = AssignmentCalendarDayID, body = list(DataObject = body), searchFields = append("AssignmentCalendarDayID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AssignmentCalendarDayOverrides
	#'
	#' This function returns a dataframe or json object of AssignmentCalendarDayOverrides
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssignmentCalendarDayOverrides. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssignmentCalendarDayOverrides.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssignmentCalendarDayOverride') to get more field paths.
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
	#' @concept Position
	#' @return A list of AssignmentCalendarDayOverrides
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAssignmentCalendarDayOverrides <- function(searchConditionsList = NULL, AssignmentCalendarDayOverrideID = F, AssignmentID = F, CalendarID = F, EmployeeID = F, IsPaid = F, IsWorkday = F, Date = F, OverrideSeconds = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "AssignmentCalendarDayOverride", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AssignmentCalendarDayOverride
	#'
	#' This function returns a dataframe or json object of an AssignmentCalendarDayOverride
	#' @param AssignmentCalendarDayOverrideID The ID of the AssignmentCalendarDayOverride to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssignmentCalendarDayOverride. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssignmentCalendarDayOverride.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssignmentCalendarDayOverride') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of AssignmentCalendarDayOverride
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAssignmentCalendarDayOverride <- function(AssignmentCalendarDayOverrideID, AssignmentID = F, CalendarID = F, EmployeeID = F, IsPaid = F, IsWorkday = F, Date = F, OverrideSeconds = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AssignmentCalendarDayOverrideID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "AssignmentCalendarDayOverride", objectId = AssignmentCalendarDayOverrideID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AssignmentCalendarDayOverride
	#'
	#' This function deletes an AssignmentCalendarDayOverride
	#' @param AssignmentCalendarDayOverrideID The ID of the AssignmentCalendarDayOverride to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The AssignmentCalendarDayOverrideID of the deleted AssignmentCalendarDayOverride.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAssignmentCalendarDayOverride <- function(AssignmentCalendarDayOverrideID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "AssignmentCalendarDayOverride", objectId = AssignmentCalendarDayOverrideID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AssignmentCalendarDayOverride
	#'
	#' This function creates an AssignmentCalendarDayOverride
	#' @param fieldNames The field values to give the created AssignmentCalendarDayOverride. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created AssignmentCalendarDayOverride
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAssignmentCalendarDayOverride <- function(AssignmentID = NULL, CalendarID = NULL, EmployeeID = NULL, IsPaid = NULL, IsWorkday = NULL, Date = NULL, OverrideSeconds = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "AssignmentCalendarDayOverride", body = list(DataObject = body), searchFields = append("AssignmentCalendarDayOverrideID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AssignmentCalendarDayOverride
	#'
	#' This function modifies an AssignmentCalendarDayOverride
	#' @param fieldNames The field values to give the modified AssignmentCalendarDayOverride. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified AssignmentCalendarDayOverride
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAssignmentCalendarDayOverride <- function(AssignmentCalendarDayOverrideID, AssignmentID = NULL, CalendarID = NULL, EmployeeID = NULL, IsPaid = NULL, IsWorkday = NULL, Date = NULL, OverrideSeconds = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "AssignmentCalendarDayOverride", objectId = AssignmentCalendarDayOverrideID, body = list(DataObject = body), searchFields = append("AssignmentCalendarDayOverrideID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List OrganizationChartApprovers
	#'
	#' This function returns a dataframe or json object of OrganizationChartApprovers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OrganizationChartApprovers. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OrganizationChartApprovers.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OrganizationChartApprover') to get more field paths.
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
	#' @concept Position
	#' @return A list of OrganizationChartApprovers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listOrganizationChartApprovers <- function(searchConditionsList = NULL, OrganizationChartID = F, OrganizationChartRelationshipIDSupervisor = F, OrganizationChartRelationshipIDEmployee = F, PositionDistributionID = F, LevelsBelowSupervisor = F, IsFinalApproval = F, PositionIDSupervisor = F, PositionIDEmployee = F, IsApprovalPathForApprovalObject = F, ApprovalStatusForApprovalObject = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "OrganizationChartApprover", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an OrganizationChartApprover
	#'
	#' This function returns a dataframe or json object of an OrganizationChartApprover
	#' @param OrganizationChartApproverID The ID of the OrganizationChartApprover to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OrganizationChartApprover. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OrganizationChartApprover.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OrganizationChartApprover') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of OrganizationChartApprover
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getOrganizationChartApprover <- function(OrganizationChartApproverID, OrganizationChartID = F, OrganizationChartRelationshipIDSupervisor = F, OrganizationChartRelationshipIDEmployee = F, PositionDistributionID = F, LevelsBelowSupervisor = F, IsFinalApproval = F, PositionIDSupervisor = F, PositionIDEmployee = F, IsApprovalPathForApprovalObject = F, ApprovalStatusForApprovalObject = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "OrganizationChartApproverID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "OrganizationChartApprover", objectId = OrganizationChartApproverID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an OrganizationChartApprover
	#'
	#' This function deletes an OrganizationChartApprover
	#' @param OrganizationChartApproverID The ID of the OrganizationChartApprover to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The OrganizationChartApproverID of the deleted OrganizationChartApprover.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteOrganizationChartApprover <- function(OrganizationChartApproverID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "OrganizationChartApprover", objectId = OrganizationChartApproverID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an OrganizationChartApprover
	#'
	#' This function creates an OrganizationChartApprover
	#' @param fieldNames The field values to give the created OrganizationChartApprover. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created OrganizationChartApprover
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createOrganizationChartApprover <- function(OrganizationChartID = NULL, OrganizationChartRelationshipIDEmployee = NULL, PositionDistributionID = NULL, LevelsBelowSupervisor = NULL, IsFinalApproval = NULL, PositionIDSupervisor = NULL, PositionIDEmployee = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "OrganizationChartApprover", body = list(DataObject = body), searchFields = append("OrganizationChartApproverID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an OrganizationChartApprover
	#'
	#' This function modifies an OrganizationChartApprover
	#' @param fieldNames The field values to give the modified OrganizationChartApprover. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified OrganizationChartApprover
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyOrganizationChartApprover <- function(OrganizationChartApproverID, OrganizationChartID = NULL, OrganizationChartRelationshipIDEmployee = NULL, PositionDistributionID = NULL, LevelsBelowSupervisor = NULL, IsFinalApproval = NULL, PositionIDSupervisor = NULL, PositionIDEmployee = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "OrganizationChartApprover", objectId = OrganizationChartApproverID, body = list(DataObject = body), searchFields = append("OrganizationChartApproverID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List OrganizationChartRelationshipPositionDistributions
	#'
	#' This function returns a dataframe or json object of OrganizationChartRelationshipPositionDistributions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OrganizationChartRelationshipPositionDistributions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OrganizationChartRelationshipPositionDistributions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OrganizationChartRelationshipPositionDistribution') to get more field paths.
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
	#' @concept Position
	#' @return A list of OrganizationChartRelationshipPositionDistributions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listOrganizationChartRelationshipPositionDistributions <- function(searchConditionsList = NULL, OrganizationChartRelationshipPositionDistributionID = F, OrganizationChartRelationshipID = F, PositionDistributionID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "OrganizationChartRelationshipPositionDistribution", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an OrganizationChartRelationshipPositionDistribution
	#'
	#' This function returns a dataframe or json object of an OrganizationChartRelationshipPositionDistribution
	#' @param OrganizationChartRelationshipPositionDistributionID The ID of the OrganizationChartRelationshipPositionDistribution to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OrganizationChartRelationshipPositionDistribution. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OrganizationChartRelationshipPositionDistribution.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OrganizationChartRelationshipPositionDistribution') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of OrganizationChartRelationshipPositionDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getOrganizationChartRelationshipPositionDistribution <- function(OrganizationChartRelationshipPositionDistributionID, OrganizationChartRelationshipID = F, PositionDistributionID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "OrganizationChartRelationshipPositionDistributionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "OrganizationChartRelationshipPositionDistribution", objectId = OrganizationChartRelationshipPositionDistributionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an OrganizationChartRelationshipPositionDistribution
	#'
	#' This function deletes an OrganizationChartRelationshipPositionDistribution
	#' @param OrganizationChartRelationshipPositionDistributionID The ID of the OrganizationChartRelationshipPositionDistribution to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The OrganizationChartRelationshipPositionDistributionID of the deleted OrganizationChartRelationshipPositionDistribution.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteOrganizationChartRelationshipPositionDistribution <- function(OrganizationChartRelationshipPositionDistributionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "OrganizationChartRelationshipPositionDistribution", objectId = OrganizationChartRelationshipPositionDistributionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an OrganizationChartRelationshipPositionDistribution
	#'
	#' This function creates an OrganizationChartRelationshipPositionDistribution
	#' @param fieldNames The field values to give the created OrganizationChartRelationshipPositionDistribution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created OrganizationChartRelationshipPositionDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createOrganizationChartRelationshipPositionDistribution <- function(OrganizationChartRelationshipID = NULL, PositionDistributionID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "OrganizationChartRelationshipPositionDistribution", body = list(DataObject = body), searchFields = append("OrganizationChartRelationshipPositionDistributionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an OrganizationChartRelationshipPositionDistribution
	#'
	#' This function modifies an OrganizationChartRelationshipPositionDistribution
	#' @param fieldNames The field values to give the modified OrganizationChartRelationshipPositionDistribution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified OrganizationChartRelationshipPositionDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyOrganizationChartRelationshipPositionDistribution <- function(OrganizationChartRelationshipPositionDistributionID, OrganizationChartRelationshipID = NULL, PositionDistributionID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "OrganizationChartRelationshipPositionDistribution", objectId = OrganizationChartRelationshipPositionDistributionID, body = list(DataObject = body), searchFields = append("OrganizationChartRelationshipPositionDistributionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempOrganizationChartRelationshipPositionDistributions
	#'
	#' This function returns a dataframe or json object of TempOrganizationChartRelationshipPositionDistributions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempOrganizationChartRelationshipPositionDistributions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempOrganizationChartRelationshipPositionDistributions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempOrganizationChartRelationshipPositionDistribution') to get more field paths.
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
	#' @concept Position
	#' @return A list of TempOrganizationChartRelationshipPositionDistributions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempOrganizationChartRelationshipPositionDistributions <- function(searchConditionsList = NULL, TempOrganizationChartRelationshipPositionDistributionID = F, TempType = F, OrganizationChartRelationshipPositionDistributionID = F, OrganizationChartRelationshipID = F, OrganizationChartCode = F, OrganizationChartDescription = F, SupervisorEmployeeNumber = F, SupervisorFullName = F, SupervisorPositionTypeCode = F, SupervisorPositionTypeDescription = F, SupervisorAssignmentTypeCodes = F, SupervisorBuildingCodes = F, PositionDistributionID = F, EmployeePositionNumber = F, EmployeeAssignmentTypeCode = F, EmployeeAssignmentTypeDescription = F, EmployeeBuildingCode = F, EmployeeBuildingDescription = F, EmployeeFTEGroupCode = F, EmployeeFTEGroupDescription = F, EmployeeNumber = F, EmployeeFullName = F, ErrorCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempOrganizationChartRelationshipPositionDistribution", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempOrganizationChartRelationshipPositionDistribution
	#'
	#' This function returns a dataframe or json object of a TempOrganizationChartRelationshipPositionDistribution
	#' @param TempOrganizationChartRelationshipPositionDistributionID The ID of the TempOrganizationChartRelationshipPositionDistribution to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempOrganizationChartRelationshipPositionDistribution. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempOrganizationChartRelationshipPositionDistribution.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempOrganizationChartRelationshipPositionDistribution') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A dataframe or of TempOrganizationChartRelationshipPositionDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempOrganizationChartRelationshipPositionDistribution <- function(TempOrganizationChartRelationshipPositionDistributionID, TempType = F, OrganizationChartRelationshipPositionDistributionID = F, OrganizationChartRelationshipID = F, OrganizationChartCode = F, OrganizationChartDescription = F, SupervisorEmployeeNumber = F, SupervisorFullName = F, SupervisorPositionTypeCode = F, SupervisorPositionTypeDescription = F, SupervisorAssignmentTypeCodes = F, SupervisorBuildingCodes = F, PositionDistributionID = F, EmployeePositionNumber = F, EmployeeAssignmentTypeCode = F, EmployeeAssignmentTypeDescription = F, EmployeeBuildingCode = F, EmployeeBuildingDescription = F, EmployeeFTEGroupCode = F, EmployeeFTEGroupDescription = F, EmployeeNumber = F, EmployeeFullName = F, ErrorCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempOrganizationChartRelationshipPositionDistributionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Position", objectName = "TempOrganizationChartRelationshipPositionDistribution", objectId = TempOrganizationChartRelationshipPositionDistributionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempOrganizationChartRelationshipPositionDistribution
	#'
	#' This function deletes a TempOrganizationChartRelationshipPositionDistribution
	#' @param TempOrganizationChartRelationshipPositionDistributionID The ID of the TempOrganizationChartRelationshipPositionDistribution to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The TempOrganizationChartRelationshipPositionDistributionID of the deleted TempOrganizationChartRelationshipPositionDistribution.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempOrganizationChartRelationshipPositionDistribution <- function(TempOrganizationChartRelationshipPositionDistributionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Position", objectName = "TempOrganizationChartRelationshipPositionDistribution", objectId = TempOrganizationChartRelationshipPositionDistributionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempOrganizationChartRelationshipPositionDistribution
	#'
	#' This function creates a TempOrganizationChartRelationshipPositionDistribution
	#' @param fieldNames The field values to give the created TempOrganizationChartRelationshipPositionDistribution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return A newly created TempOrganizationChartRelationshipPositionDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempOrganizationChartRelationshipPositionDistribution <- function(TempType = NULL, OrganizationChartRelationshipPositionDistributionID = NULL, OrganizationChartRelationshipID = NULL, OrganizationChartCode = NULL, OrganizationChartDescription = NULL, SupervisorEmployeeNumber = NULL, SupervisorFullName = NULL, SupervisorPositionTypeCode = NULL, SupervisorPositionTypeDescription = NULL, SupervisorAssignmentTypeCodes = NULL, SupervisorBuildingCodes = NULL, PositionDistributionID = NULL, EmployeePositionNumber = NULL, EmployeeAssignmentTypeCode = NULL, EmployeeAssignmentTypeDescription = NULL, EmployeeBuildingCode = NULL, EmployeeBuildingDescription = NULL, EmployeeFTEGroupCode = NULL, EmployeeFTEGroupDescription = NULL, EmployeeNumber = NULL, EmployeeFullName = NULL, ErrorCount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Position", objectName = "TempOrganizationChartRelationshipPositionDistribution", body = list(DataObject = body), searchFields = append("TempOrganizationChartRelationshipPositionDistributionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempOrganizationChartRelationshipPositionDistribution
	#'
	#' This function modifies a TempOrganizationChartRelationshipPositionDistribution
	#' @param fieldNames The field values to give the modified TempOrganizationChartRelationshipPositionDistribution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Position
	#' @return The modified TempOrganizationChartRelationshipPositionDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempOrganizationChartRelationshipPositionDistribution <- function(TempOrganizationChartRelationshipPositionDistributionID, TempType = NULL, OrganizationChartRelationshipPositionDistributionID = NULL, OrganizationChartRelationshipID = NULL, OrganizationChartCode = NULL, OrganizationChartDescription = NULL, SupervisorEmployeeNumber = NULL, SupervisorFullName = NULL, SupervisorPositionTypeCode = NULL, SupervisorPositionTypeDescription = NULL, SupervisorAssignmentTypeCodes = NULL, SupervisorBuildingCodes = NULL, PositionDistributionID = NULL, EmployeePositionNumber = NULL, EmployeeAssignmentTypeCode = NULL, EmployeeAssignmentTypeDescription = NULL, EmployeeBuildingCode = NULL, EmployeeBuildingDescription = NULL, EmployeeFTEGroupCode = NULL, EmployeeFTEGroupDescription = NULL, EmployeeNumber = NULL, EmployeeFullName = NULL, ErrorCount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Position", objectName = "TempOrganizationChartRelationshipPositionDistribution", objectId = TempOrganizationChartRelationshipPositionDistributionID, body = list(DataObject = body), searchFields = append("TempOrganizationChartRelationshipPositionDistributionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}
