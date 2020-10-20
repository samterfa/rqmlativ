
	#' List LastEmployeeNumbers
	#'
	#' This function returns a dataframe or json object of LastEmployeeNumbers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given LastEmployeeNumbers. Defaults to FALSE for all return fields which, for convenience, returns all fields for the LastEmployeeNumbers.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('LastEmployeeNumber') to get more field paths.
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
	#' @concept Employee
	#' @return A list of LastEmployeeNumbers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listLastEmployeeNumbers <- function(searchConditionsList = NULL, LastEmployeeNumberID = F, EmployeeNumber = F, ConfigSystemID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "LastEmployeeNumber", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a LastEmployeeNumber
	#'
	#' This function returns a dataframe or json object of a LastEmployeeNumber
	#' @param LastEmployeeNumberID The ID of the LastEmployeeNumber to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given LastEmployeeNumber. Defaults to FALSE for all return fields which, for convenience, returns all fields for the LastEmployeeNumber.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('LastEmployeeNumber') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of LastEmployeeNumber
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getLastEmployeeNumber <- function(LastEmployeeNumberID, EmployeeNumber = F, ConfigSystemID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "LastEmployeeNumberID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "LastEmployeeNumber", objectId = LastEmployeeNumberID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a LastEmployeeNumber
	#'
	#' This function deletes a LastEmployeeNumber
	#' @param LastEmployeeNumberID The ID of the LastEmployeeNumber to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The LastEmployeeNumberID of the deleted LastEmployeeNumber.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteLastEmployeeNumber <- function(LastEmployeeNumberID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "LastEmployeeNumber", objectId = LastEmployeeNumberID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a LastEmployeeNumber
	#'
	#' This function creates a LastEmployeeNumber
	#' @param fieldNames The field values to give the created LastEmployeeNumber. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created LastEmployeeNumber
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createLastEmployeeNumber <- function(EmployeeNumber = NULL, ConfigSystemID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "LastEmployeeNumber", body = list(DataObject = body), searchFields = append("LastEmployeeNumberID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a LastEmployeeNumber
	#'
	#' This function modifies a LastEmployeeNumber
	#' @param fieldNames The field values to give the modified LastEmployeeNumber. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified LastEmployeeNumber
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyLastEmployeeNumber <- function(LastEmployeeNumberID, EmployeeNumber = NULL, ConfigSystemID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "LastEmployeeNumber", objectId = LastEmployeeNumberID, body = list(DataObject = body), searchFields = append("LastEmployeeNumberID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Employments
	#'
	#' This function returns a dataframe or json object of Employments
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Employments. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Employments.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Employment') to get more field paths.
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
	#' @concept Employee
	#' @return A list of Employments
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEmployments <- function(searchConditionsList = NULL, EmploymentID = F, DistrictID = F, EmployeeID = F, HireDate = F, StartDate = F, EndDate = F, TerminationID = F, Comment = F, EmploymentStatusID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsTerminated = F, EmploymentYears = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "Employment", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an Employment
	#'
	#' This function returns a dataframe or json object of an Employment
	#' @param EmploymentID The ID of the Employment to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Employment. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Employment.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Employment') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of Employment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEmployment <- function(EmploymentID, DistrictID = F, EmployeeID = F, HireDate = F, StartDate = F, EndDate = F, TerminationID = F, Comment = F, EmploymentStatusID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsTerminated = F, EmploymentYears = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EmploymentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "Employment", objectId = EmploymentID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an Employment
	#'
	#' This function deletes an Employment
	#' @param EmploymentID The ID of the Employment to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The EmploymentID of the deleted Employment.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEmployment <- function(EmploymentID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "Employment", objectId = EmploymentID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an Employment
	#'
	#' This function creates an Employment
	#' @param fieldNames The field values to give the created Employment. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created Employment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEmployment <- function(DistrictID = NULL, EmployeeID = NULL, HireDate = NULL, StartDate = NULL, EndDate = NULL, TerminationID = NULL, Comment = NULL, EmploymentStatusID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "Employment", body = list(DataObject = body), searchFields = append("EmploymentID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an Employment
	#'
	#' This function modifies an Employment
	#' @param fieldNames The field values to give the modified Employment. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified Employment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEmployment <- function(EmploymentID, DistrictID = NULL, EmployeeID = NULL, HireDate = NULL, StartDate = NULL, EndDate = NULL, TerminationID = NULL, Comment = NULL, EmploymentStatusID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "Employment", objectId = EmploymentID, body = list(DataObject = body), searchFields = append("EmploymentID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StateYearExperienceLabelMNS
	#'
	#' This function returns a dataframe or json object of StateYearExperienceLabelMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StateYearExperienceLabelMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StateYearExperienceLabelMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StateYearExperienceLabelMN') to get more field paths.
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
	#' @concept Employee
	#' @return A list of StateYearExperienceLabelMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStateYearExperienceLabelMNS <- function(searchConditionsList = NULL, StateYearExperienceLabelMNID = F, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Code = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "StateYearExperienceLabelMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StateYearExperienceLabelMN
	#'
	#' This function returns a dataframe or json object of a StateYearExperienceLabelMN
	#' @param StateYearExperienceLabelMNID The ID of the StateYearExperienceLabelMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StateYearExperienceLabelMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StateYearExperienceLabelMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StateYearExperienceLabelMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of StateYearExperienceLabelMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStateYearExperienceLabelMN <- function(StateYearExperienceLabelMNID, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Code = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StateYearExperienceLabelMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "StateYearExperienceLabelMN", objectId = StateYearExperienceLabelMNID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StateYearExperienceLabelMN
	#'
	#' This function deletes a StateYearExperienceLabelMN
	#' @param StateYearExperienceLabelMNID The ID of the StateYearExperienceLabelMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The StateYearExperienceLabelMNID of the deleted StateYearExperienceLabelMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStateYearExperienceLabelMN <- function(StateYearExperienceLabelMNID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "StateYearExperienceLabelMN", objectId = StateYearExperienceLabelMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StateYearExperienceLabelMN
	#'
	#' This function creates a StateYearExperienceLabelMN
	#' @param fieldNames The field values to give the created StateYearExperienceLabelMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created StateYearExperienceLabelMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStateYearExperienceLabelMN <- function(Description = NULL, Code = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "StateYearExperienceLabelMN", body = list(DataObject = body), searchFields = append("StateYearExperienceLabelMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StateYearExperienceLabelMN
	#'
	#' This function modifies a StateYearExperienceLabelMN
	#' @param fieldNames The field values to give the modified StateYearExperienceLabelMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified StateYearExperienceLabelMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStateYearExperienceLabelMN <- function(StateYearExperienceLabelMNID, Description = NULL, Code = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "StateYearExperienceLabelMN", objectId = StateYearExperienceLabelMNID, body = list(DataObject = body), searchFields = append("StateYearExperienceLabelMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List YearExperienceLabels
	#'
	#' This function returns a dataframe or json object of YearExperienceLabels
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given YearExperienceLabels. Defaults to FALSE for all return fields which, for convenience, returns all fields for the YearExperienceLabels.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('YearExperienceLabel') to get more field paths.
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
	#' @concept Employee
	#' @return A list of YearExperienceLabels
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listYearExperienceLabels <- function(searchConditionsList = NULL, YearExperienceLabelMNID = F, StateYearExperienceLabelMNID = F, YearExperienceLabelID = F, Name = F, DisplayOrder = F, FormattedYearExperienceLabel = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsCRDCTeacherExperience = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "YearExperienceLabel", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a YearExperienceLabel
	#'
	#' This function returns a dataframe or json object of a YearExperienceLabel
	#' @param YearExperienceLabelID The ID of the YearExperienceLabel to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given YearExperienceLabel. Defaults to FALSE for all return fields which, for convenience, returns all fields for the YearExperienceLabel.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('YearExperienceLabel') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of YearExperienceLabel
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getYearExperienceLabel <- function(YearExperienceLabelID, YearExperienceLabelMNID = F, StateYearExperienceLabelMNID = F, Name = F, DisplayOrder = F, FormattedYearExperienceLabel = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsCRDCTeacherExperience = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "YearExperienceLabelID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "YearExperienceLabel", objectId = YearExperienceLabelID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a YearExperienceLabel
	#'
	#' This function deletes a YearExperienceLabel
	#' @param YearExperienceLabelID The ID of the YearExperienceLabel to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The YearExperienceLabelID of the deleted YearExperienceLabel.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteYearExperienceLabel <- function(YearExperienceLabelID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "YearExperienceLabel", objectId = YearExperienceLabelID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a YearExperienceLabel
	#'
	#' This function creates a YearExperienceLabel
	#' @param fieldNames The field values to give the created YearExperienceLabel. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created YearExperienceLabel
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createYearExperienceLabel <- function(StateYearExperienceLabelMNID = NULL, Name = NULL, DisplayOrder = NULL, IsCRDCTeacherExperience = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "YearExperienceLabel", body = list(DataObject = body), searchFields = append("YearExperienceLabelID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a YearExperienceLabel
	#'
	#' This function modifies a YearExperienceLabel
	#' @param fieldNames The field values to give the modified YearExperienceLabel. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified YearExperienceLabel
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyYearExperienceLabel <- function(YearExperienceLabelID, StateYearExperienceLabelMNID = NULL, Name = NULL, DisplayOrder = NULL, IsCRDCTeacherExperience = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "YearExperienceLabel", objectId = YearExperienceLabelID, body = list(DataObject = body), searchFields = append("YearExperienceLabelID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DegreeTypes
	#'
	#' This function returns a dataframe or json object of DegreeTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DegreeTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DegreeTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DegreeType') to get more field paths.
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
	#' @concept Employee
	#' @return A list of DegreeTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDegreeTypes <- function(searchConditionsList = NULL, DegreeTypeMNID = F, StateSTARHighestEducationLevelMNID = F, DegreeTypeID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Rank = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "DegreeType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DegreeType
	#'
	#' This function returns a dataframe or json object of a DegreeType
	#' @param DegreeTypeID The ID of the DegreeType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DegreeType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DegreeType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DegreeType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of DegreeType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDegreeType <- function(DegreeTypeID, DegreeTypeMNID = F, StateSTARHighestEducationLevelMNID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Rank = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DegreeTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "DegreeType", objectId = DegreeTypeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DegreeType
	#'
	#' This function deletes a DegreeType
	#' @param DegreeTypeID The ID of the DegreeType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The DegreeTypeID of the deleted DegreeType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDegreeType <- function(DegreeTypeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "DegreeType", objectId = DegreeTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DegreeType
	#'
	#' This function creates a DegreeType
	#' @param fieldNames The field values to give the created DegreeType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created DegreeType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDegreeType <- function(StateSTARHighestEducationLevelMNID = NULL, Code = NULL, Description = NULL, Rank = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "DegreeType", body = list(DataObject = body), searchFields = append("DegreeTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DegreeType
	#'
	#' This function modifies a DegreeType
	#' @param fieldNames The field values to give the modified DegreeType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified DegreeType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDegreeType <- function(DegreeTypeID, StateSTARHighestEducationLevelMNID = NULL, Code = NULL, Description = NULL, Rank = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "DegreeType", objectId = DegreeTypeID, body = list(DataObject = body), searchFields = append("DegreeTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Employees
	#'
	#' This function returns a dataframe or json object of Employees
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Employees. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Employees.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Employee') to get more field paths.
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
	#' @concept Employee
	#' @return A list of Employees
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEmployees <- function(searchConditionsList = NULL, EmployeeID = F, EmployeeNumber = F, AllowEmployeeAccess = F, NameID = F, FullNameLFM = F, FullNameFL = F, FullNameFML = F, RenderButtonOnSystemWide = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeThirdPartyImportID = F, ConversionKey = F, EmployeeNumberStored = F, MostRecentFileFolderNumber = F, EmployeeMNID = F, STARUniqueIdentifier = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "Employee", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an Employee
	#'
	#' This function returns a dataframe or json object of an Employee
	#' @param EmployeeID The ID of the Employee to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Employee. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Employee.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Employee') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of Employee
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEmployee <- function(EmployeeID, EmployeeNumber = F, AllowEmployeeAccess = F, NameID = F, FullNameLFM = F, FullNameFL = F, FullNameFML = F, RenderButtonOnSystemWide = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeThirdPartyImportID = F, ConversionKey = F, EmployeeNumberStored = F, MostRecentFileFolderNumber = F, EmployeeMNID = F, STARUniqueIdentifier = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EmployeeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "Employee", objectId = EmployeeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an Employee
	#'
	#' This function deletes an Employee
	#' @param EmployeeID The ID of the Employee to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The EmployeeID of the deleted Employee.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEmployee <- function(EmployeeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "Employee", objectId = EmployeeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an Employee
	#'
	#' This function creates an Employee
	#' @param fieldNames The field values to give the created Employee. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created Employee
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEmployee <- function(AllowEmployeeAccess = NULL, NameID = NULL, EmployeeThirdPartyImportID = NULL, ConversionKey = NULL, EmployeeNumberStored = NULL, STARUniqueIdentifier = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "Employee", body = list(DataObject = body), searchFields = append("EmployeeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an Employee
	#'
	#' This function modifies an Employee
	#' @param fieldNames The field values to give the modified Employee. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified Employee
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEmployee <- function(EmployeeID, AllowEmployeeAccess = NULL, NameID = NULL, EmployeeThirdPartyImportID = NULL, ConversionKey = NULL, EmployeeNumberStored = NULL, STARUniqueIdentifier = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "Employee", objectId = EmployeeID, body = list(DataObject = body), searchFields = append("EmployeeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StateSTARInactiveTransferTerminationMNS
	#'
	#' This function returns a dataframe or json object of StateSTARInactiveTransferTerminationMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StateSTARInactiveTransferTerminationMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StateSTARInactiveTransferTerminationMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StateSTARInactiveTransferTerminationMN') to get more field paths.
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
	#' @concept Employee
	#' @return A list of StateSTARInactiveTransferTerminationMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStateSTARInactiveTransferTerminationMNS <- function(searchConditionsList = NULL, StateSTARInactiveTransferTerminationMNID = F, Code = F, Description = F, CodeDescription = F, Type = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "StateSTARInactiveTransferTerminationMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StateSTARInactiveTransferTerminationMN
	#'
	#' This function returns a dataframe or json object of a StateSTARInactiveTransferTerminationMN
	#' @param StateSTARInactiveTransferTerminationMNID The ID of the StateSTARInactiveTransferTerminationMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StateSTARInactiveTransferTerminationMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StateSTARInactiveTransferTerminationMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StateSTARInactiveTransferTerminationMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of StateSTARInactiveTransferTerminationMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStateSTARInactiveTransferTerminationMN <- function(StateSTARInactiveTransferTerminationMNID, Code = F, Description = F, CodeDescription = F, Type = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StateSTARInactiveTransferTerminationMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "StateSTARInactiveTransferTerminationMN", objectId = StateSTARInactiveTransferTerminationMNID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StateSTARInactiveTransferTerminationMN
	#'
	#' This function deletes a StateSTARInactiveTransferTerminationMN
	#' @param StateSTARInactiveTransferTerminationMNID The ID of the StateSTARInactiveTransferTerminationMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The StateSTARInactiveTransferTerminationMNID of the deleted StateSTARInactiveTransferTerminationMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStateSTARInactiveTransferTerminationMN <- function(StateSTARInactiveTransferTerminationMNID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "StateSTARInactiveTransferTerminationMN", objectId = StateSTARInactiveTransferTerminationMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StateSTARInactiveTransferTerminationMN
	#'
	#' This function creates a StateSTARInactiveTransferTerminationMN
	#' @param fieldNames The field values to give the created StateSTARInactiveTransferTerminationMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created StateSTARInactiveTransferTerminationMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStateSTARInactiveTransferTerminationMN <- function(Code = NULL, Description = NULL, Type = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "StateSTARInactiveTransferTerminationMN", body = list(DataObject = body), searchFields = append("StateSTARInactiveTransferTerminationMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StateSTARInactiveTransferTerminationMN
	#'
	#' This function modifies a StateSTARInactiveTransferTerminationMN
	#' @param fieldNames The field values to give the modified StateSTARInactiveTransferTerminationMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified StateSTARInactiveTransferTerminationMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStateSTARInactiveTransferTerminationMN <- function(StateSTARInactiveTransferTerminationMNID, Code = NULL, Description = NULL, Type = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "StateSTARInactiveTransferTerminationMN", objectId = StateSTARInactiveTransferTerminationMNID, body = list(DataObject = body), searchFields = append("StateSTARInactiveTransferTerminationMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StateSTARNewLicensedStaffMNS
	#'
	#' This function returns a dataframe or json object of StateSTARNewLicensedStaffMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StateSTARNewLicensedStaffMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StateSTARNewLicensedStaffMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StateSTARNewLicensedStaffMN') to get more field paths.
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
	#' @concept Employee
	#' @return A list of StateSTARNewLicensedStaffMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStateSTARNewLicensedStaffMNS <- function(searchConditionsList = NULL, StateSTARNewLicensedStaffMNID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "StateSTARNewLicensedStaffMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StateSTARNewLicensedStaffMN
	#'
	#' This function returns a dataframe or json object of a StateSTARNewLicensedStaffMN
	#' @param StateSTARNewLicensedStaffMNID The ID of the StateSTARNewLicensedStaffMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StateSTARNewLicensedStaffMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StateSTARNewLicensedStaffMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StateSTARNewLicensedStaffMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of StateSTARNewLicensedStaffMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStateSTARNewLicensedStaffMN <- function(StateSTARNewLicensedStaffMNID, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StateSTARNewLicensedStaffMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "StateSTARNewLicensedStaffMN", objectId = StateSTARNewLicensedStaffMNID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StateSTARNewLicensedStaffMN
	#'
	#' This function deletes a StateSTARNewLicensedStaffMN
	#' @param StateSTARNewLicensedStaffMNID The ID of the StateSTARNewLicensedStaffMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The StateSTARNewLicensedStaffMNID of the deleted StateSTARNewLicensedStaffMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStateSTARNewLicensedStaffMN <- function(StateSTARNewLicensedStaffMNID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "StateSTARNewLicensedStaffMN", objectId = StateSTARNewLicensedStaffMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StateSTARNewLicensedStaffMN
	#'
	#' This function creates a StateSTARNewLicensedStaffMN
	#' @param fieldNames The field values to give the created StateSTARNewLicensedStaffMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created StateSTARNewLicensedStaffMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStateSTARNewLicensedStaffMN <- function(Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "StateSTARNewLicensedStaffMN", body = list(DataObject = body), searchFields = append("StateSTARNewLicensedStaffMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StateSTARNewLicensedStaffMN
	#'
	#' This function modifies a StateSTARNewLicensedStaffMN
	#' @param fieldNames The field values to give the modified StateSTARNewLicensedStaffMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified StateSTARNewLicensedStaffMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStateSTARNewLicensedStaffMN <- function(StateSTARNewLicensedStaffMNID, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "StateSTARNewLicensedStaffMN", objectId = StateSTARNewLicensedStaffMNID, body = list(DataObject = body), searchFields = append("StateSTARNewLicensedStaffMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StateSTARNewNonLicensedStaffMNS
	#'
	#' This function returns a dataframe or json object of StateSTARNewNonLicensedStaffMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StateSTARNewNonLicensedStaffMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StateSTARNewNonLicensedStaffMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StateSTARNewNonLicensedStaffMN') to get more field paths.
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
	#' @concept Employee
	#' @return A list of StateSTARNewNonLicensedStaffMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStateSTARNewNonLicensedStaffMNS <- function(searchConditionsList = NULL, StateSTARNewNonLicensedStaffMNID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "StateSTARNewNonLicensedStaffMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StateSTARNewNonLicensedStaffMN
	#'
	#' This function returns a dataframe or json object of a StateSTARNewNonLicensedStaffMN
	#' @param StateSTARNewNonLicensedStaffMNID The ID of the StateSTARNewNonLicensedStaffMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StateSTARNewNonLicensedStaffMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StateSTARNewNonLicensedStaffMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StateSTARNewNonLicensedStaffMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of StateSTARNewNonLicensedStaffMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStateSTARNewNonLicensedStaffMN <- function(StateSTARNewNonLicensedStaffMNID, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StateSTARNewNonLicensedStaffMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "StateSTARNewNonLicensedStaffMN", objectId = StateSTARNewNonLicensedStaffMNID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StateSTARNewNonLicensedStaffMN
	#'
	#' This function deletes a StateSTARNewNonLicensedStaffMN
	#' @param StateSTARNewNonLicensedStaffMNID The ID of the StateSTARNewNonLicensedStaffMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The StateSTARNewNonLicensedStaffMNID of the deleted StateSTARNewNonLicensedStaffMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStateSTARNewNonLicensedStaffMN <- function(StateSTARNewNonLicensedStaffMNID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "StateSTARNewNonLicensedStaffMN", objectId = StateSTARNewNonLicensedStaffMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StateSTARNewNonLicensedStaffMN
	#'
	#' This function creates a StateSTARNewNonLicensedStaffMN
	#' @param fieldNames The field values to give the created StateSTARNewNonLicensedStaffMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created StateSTARNewNonLicensedStaffMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStateSTARNewNonLicensedStaffMN <- function(Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "StateSTARNewNonLicensedStaffMN", body = list(DataObject = body), searchFields = append("StateSTARNewNonLicensedStaffMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StateSTARNewNonLicensedStaffMN
	#'
	#' This function modifies a StateSTARNewNonLicensedStaffMN
	#' @param fieldNames The field values to give the modified StateSTARNewNonLicensedStaffMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified StateSTARNewNonLicensedStaffMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStateSTARNewNonLicensedStaffMN <- function(StateSTARNewNonLicensedStaffMNID, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "StateSTARNewNonLicensedStaffMN", objectId = StateSTARNewNonLicensedStaffMNID, body = list(DataObject = body), searchFields = append("StateSTARNewNonLicensedStaffMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EmployeeConfigSystems
	#'
	#' This function returns a dataframe or json object of EmployeeConfigSystems
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeConfigSystems. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeConfigSystems.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeConfigSystem') to get more field paths.
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
	#' @concept Employee
	#' @return A list of EmployeeConfigSystems
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEmployeeConfigSystems <- function(searchConditionsList = NULL, ConfigSystemID = F, AutoGenerateSecurityUser = F, AllowEmployeeAccessDefault = F, AutoGenerateEmail = F, EmailTypeIDDefault = F, Domain = F, NewEmployeeAccessUserMessageContent = F, NewEmployeeAccessUserMessageSubject = F, AutoGenerateEmployeeNumber = F, AutoCreateVendorOnEmployeeAdd = F, EmployeeNumberLength = F, EmployeeNumberGenerateOption = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "ConfigSystem", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EmployeeConfigSystem
	#'
	#' This function returns a dataframe or json object of an EmployeeConfigSystem
	#' @param EmployeeConfigSystemID The ID of the EmployeeConfigSystem to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeConfigSystem. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeConfigSystem.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeConfigSystem') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of EmployeeConfigSystem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEmployeeConfigSystem <- function(EmployeeConfigSystemID, ConfigSystemID = F, AutoGenerateSecurityUser = F, AllowEmployeeAccessDefault = F, AutoGenerateEmail = F, EmailTypeIDDefault = F, Domain = F, NewEmployeeAccessUserMessageContent = F, NewEmployeeAccessUserMessageSubject = F, AutoGenerateEmployeeNumber = F, AutoCreateVendorOnEmployeeAdd = F, EmployeeNumberLength = F, EmployeeNumberGenerateOption = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EmployeeConfigSystemID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "ConfigSystem", objectId = EmployeeConfigSystemID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EmployeeConfigSystem
	#'
	#' This function deletes an EmployeeConfigSystem
	#' @param EmployeeConfigSystemID The ID of the EmployeeConfigSystem to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The EmployeeConfigSystemID of the deleted EmployeeConfigSystem.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEmployeeConfigSystem <- function(EmployeeConfigSystemID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "ConfigSystem", objectId = EmployeeConfigSystemID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EmployeeConfigSystem
	#'
	#' This function creates an EmployeeConfigSystem
	#' @param fieldNames The field values to give the created EmployeeConfigSystem. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created EmployeeConfigSystem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEmployeeConfigSystem <- function(AutoGenerateSecurityUser = NULL, AllowEmployeeAccessDefault = NULL, AutoGenerateEmail = NULL, EmailTypeIDDefault = NULL, Domain = NULL, NewEmployeeAccessUserMessageContent = NULL, NewEmployeeAccessUserMessageSubject = NULL, AutoGenerateEmployeeNumber = NULL, AutoCreateVendorOnEmployeeAdd = NULL, EmployeeNumberLength = NULL, EmployeeNumberGenerateOption = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "ConfigSystem", body = list(DataObject = body), searchFields = append("ConfigSystemID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EmployeeConfigSystem
	#'
	#' This function modifies an EmployeeConfigSystem
	#' @param fieldNames The field values to give the modified EmployeeConfigSystem. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified EmployeeConfigSystem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEmployeeConfigSystem <- function(ConfigSystemID, AutoGenerateSecurityUser = NULL, AllowEmployeeAccessDefault = NULL, AutoGenerateEmail = NULL, EmailTypeIDDefault = NULL, Domain = NULL, NewEmployeeAccessUserMessageContent = NULL, NewEmployeeAccessUserMessageSubject = NULL, AutoGenerateEmployeeNumber = NULL, AutoCreateVendorOnEmployeeAdd = NULL, EmployeeNumberLength = NULL, EmployeeNumberGenerateOption = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "ConfigSystem", objectId = ConfigSystemID, body = list(DataObject = body), searchFields = append("ConfigSystemID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EmployeeDistricts
	#'
	#' This function returns a dataframe or json object of EmployeeDistricts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeDistricts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeDistricts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeDistrict') to get more field paths.
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
	#' @concept Employee
	#' @return A list of EmployeeDistricts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEmployeeDistricts <- function(searchConditionsList = NULL, EmployeeDistrictID = F, EmployeeID = F, DistrictID = F, CheckLocationID = F, I9Date = F, ACHAccountIDNet = F, EmployeeTimeTrackingGroupID = F, PrintACHAdviceOfDeposit = F, IsActive = F, TaxableLifeInsuranceContractCoverage = F, HireDateOriginal = F, StartDateOriginal = F, W4Date = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DeclinedFamilyMedicalCareCoverage = F, EmployeeDistrictMNID = F, ACAEmployerProvidedSelfInsuredCoverageCurrentYear = F, IncludedInACA = F, TRAExtractHash = F, IsEEOCFullTime = F, EmployerPrintedW2 = F, EmployerPrinted1095 = F, CheckTransactionPayGrossCYTD = F, DecreasesUnemploymentCompensationDeductionTransactionsCYTD = F, IsBLSFaculty = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "EmployeeDistrict", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EmployeeDistrict
	#'
	#' This function returns a dataframe or json object of an EmployeeDistrict
	#' @param EmployeeDistrictID The ID of the EmployeeDistrict to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeDistrict. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeDistrict.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeDistrict') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of EmployeeDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEmployeeDistrict <- function(EmployeeDistrictID, EmployeeID = F, DistrictID = F, CheckLocationID = F, I9Date = F, ACHAccountIDNet = F, EmployeeTimeTrackingGroupID = F, PrintACHAdviceOfDeposit = F, IsActive = F, TaxableLifeInsuranceContractCoverage = F, HireDateOriginal = F, StartDateOriginal = F, W4Date = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DeclinedFamilyMedicalCareCoverage = F, EmployeeDistrictMNID = F, ACAEmployerProvidedSelfInsuredCoverageCurrentYear = F, IncludedInACA = F, TRAExtractHash = F, IsEEOCFullTime = F, EmployerPrintedW2 = F, EmployerPrinted1095 = F, CheckTransactionPayGrossCYTD = F, DecreasesUnemploymentCompensationDeductionTransactionsCYTD = F, IsBLSFaculty = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EmployeeDistrictID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "EmployeeDistrict", objectId = EmployeeDistrictID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EmployeeDistrict
	#'
	#' This function deletes an EmployeeDistrict
	#' @param EmployeeDistrictID The ID of the EmployeeDistrict to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The EmployeeDistrictID of the deleted EmployeeDistrict.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEmployeeDistrict <- function(EmployeeDistrictID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "EmployeeDistrict", objectId = EmployeeDistrictID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EmployeeDistrict
	#'
	#' This function creates an EmployeeDistrict
	#' @param fieldNames The field values to give the created EmployeeDistrict. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created EmployeeDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEmployeeDistrict <- function(EmployeeID = NULL, DistrictID = NULL, CheckLocationID = NULL, I9Date = NULL, ACHAccountIDNet = NULL, EmployeeTimeTrackingGroupID = NULL, PrintACHAdviceOfDeposit = NULL, W4Date = NULL, DeclinedFamilyMedicalCareCoverage = NULL, IsEEOCFullTime = NULL, EmployerPrintedW2 = NULL, EmployerPrinted1095 = NULL, IsBLSFaculty = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "EmployeeDistrict", body = list(DataObject = body), searchFields = append("EmployeeDistrictID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EmployeeDistrict
	#'
	#' This function modifies an EmployeeDistrict
	#' @param fieldNames The field values to give the modified EmployeeDistrict. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified EmployeeDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEmployeeDistrict <- function(EmployeeDistrictID, EmployeeID = NULL, DistrictID = NULL, CheckLocationID = NULL, I9Date = NULL, ACHAccountIDNet = NULL, EmployeeTimeTrackingGroupID = NULL, PrintACHAdviceOfDeposit = NULL, W4Date = NULL, DeclinedFamilyMedicalCareCoverage = NULL, IsEEOCFullTime = NULL, EmployerPrintedW2 = NULL, EmployerPrinted1095 = NULL, IsBLSFaculty = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "EmployeeDistrict", objectId = EmployeeDistrictID, body = list(DataObject = body), searchFields = append("EmployeeDistrictID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Evaluations
	#'
	#' This function returns a dataframe or json object of Evaluations
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Evaluations. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Evaluations.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Evaluation') to get more field paths.
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
	#' @concept Employee
	#' @return A list of Evaluations
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEvaluations <- function(searchConditionsList = NULL, EvaluationID = F, FiscalYearID = F, EmployeeID = F, NameIDEvaluator = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "Evaluation", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an Evaluation
	#'
	#' This function returns a dataframe or json object of an Evaluation
	#' @param EvaluationID The ID of the Evaluation to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Evaluation. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Evaluation.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Evaluation') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of Evaluation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEvaluation <- function(EvaluationID, FiscalYearID = F, EmployeeID = F, NameIDEvaluator = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EvaluationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "Evaluation", objectId = EvaluationID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an Evaluation
	#'
	#' This function deletes an Evaluation
	#' @param EvaluationID The ID of the Evaluation to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The EvaluationID of the deleted Evaluation.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEvaluation <- function(EvaluationID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "Evaluation", objectId = EvaluationID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an Evaluation
	#'
	#' This function creates an Evaluation
	#' @param fieldNames The field values to give the created Evaluation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created Evaluation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEvaluation <- function(FiscalYearID = NULL, EmployeeID = NULL, NameIDEvaluator = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "Evaluation", body = list(DataObject = body), searchFields = append("EvaluationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an Evaluation
	#'
	#' This function modifies an Evaluation
	#' @param fieldNames The field values to give the modified Evaluation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified Evaluation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEvaluation <- function(EvaluationID, FiscalYearID = NULL, EmployeeID = NULL, NameIDEvaluator = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "Evaluation", objectId = EvaluationID, body = list(DataObject = body), searchFields = append("EvaluationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StateTRAExemptStatusMNS
	#'
	#' This function returns a dataframe or json object of StateTRAExemptStatusMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StateTRAExemptStatusMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StateTRAExemptStatusMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StateTRAExemptStatusMN') to get more field paths.
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
	#' @concept Employee
	#' @return A list of StateTRAExemptStatusMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStateTRAExemptStatusMNS <- function(searchConditionsList = NULL, StateTRAExemptStatusMNID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "StateTRAExemptStatusMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StateTRAExemptStatusMN
	#'
	#' This function returns a dataframe or json object of a StateTRAExemptStatusMN
	#' @param StateTRAExemptStatusMNID The ID of the StateTRAExemptStatusMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StateTRAExemptStatusMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StateTRAExemptStatusMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StateTRAExemptStatusMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of StateTRAExemptStatusMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStateTRAExemptStatusMN <- function(StateTRAExemptStatusMNID, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StateTRAExemptStatusMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "StateTRAExemptStatusMN", objectId = StateTRAExemptStatusMNID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StateTRAExemptStatusMN
	#'
	#' This function deletes a StateTRAExemptStatusMN
	#' @param StateTRAExemptStatusMNID The ID of the StateTRAExemptStatusMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The StateTRAExemptStatusMNID of the deleted StateTRAExemptStatusMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStateTRAExemptStatusMN <- function(StateTRAExemptStatusMNID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "StateTRAExemptStatusMN", objectId = StateTRAExemptStatusMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StateTRAExemptStatusMN
	#'
	#' This function creates a StateTRAExemptStatusMN
	#' @param fieldNames The field values to give the created StateTRAExemptStatusMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created StateTRAExemptStatusMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStateTRAExemptStatusMN <- function(Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "StateTRAExemptStatusMN", body = list(DataObject = body), searchFields = append("StateTRAExemptStatusMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StateTRAExemptStatusMN
	#'
	#' This function modifies a StateTRAExemptStatusMN
	#' @param fieldNames The field values to give the modified StateTRAExemptStatusMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified StateTRAExemptStatusMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStateTRAExemptStatusMN <- function(StateTRAExemptStatusMNID, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "StateTRAExemptStatusMN", objectId = StateTRAExemptStatusMNID, body = list(DataObject = body), searchFields = append("StateTRAExemptStatusMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StatePERAExclusionCodeMNS
	#'
	#' This function returns a dataframe or json object of StatePERAExclusionCodeMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StatePERAExclusionCodeMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StatePERAExclusionCodeMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StatePERAExclusionCodeMN') to get more field paths.
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
	#' @concept Employee
	#' @return A list of StatePERAExclusionCodeMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStatePERAExclusionCodeMNS <- function(searchConditionsList = NULL, StatePERAExclusionCodeMNID = F, Code = F, Description = F, CodeDescription = F, MaxAmount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "StatePERAExclusionCodeMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StatePERAExclusionCodeMN
	#'
	#' This function returns a dataframe or json object of a StatePERAExclusionCodeMN
	#' @param StatePERAExclusionCodeMNID The ID of the StatePERAExclusionCodeMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StatePERAExclusionCodeMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StatePERAExclusionCodeMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StatePERAExclusionCodeMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of StatePERAExclusionCodeMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStatePERAExclusionCodeMN <- function(StatePERAExclusionCodeMNID, Code = F, Description = F, CodeDescription = F, MaxAmount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StatePERAExclusionCodeMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "StatePERAExclusionCodeMN", objectId = StatePERAExclusionCodeMNID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StatePERAExclusionCodeMN
	#'
	#' This function deletes a StatePERAExclusionCodeMN
	#' @param StatePERAExclusionCodeMNID The ID of the StatePERAExclusionCodeMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The StatePERAExclusionCodeMNID of the deleted StatePERAExclusionCodeMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStatePERAExclusionCodeMN <- function(StatePERAExclusionCodeMNID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "StatePERAExclusionCodeMN", objectId = StatePERAExclusionCodeMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StatePERAExclusionCodeMN
	#'
	#' This function creates a StatePERAExclusionCodeMN
	#' @param fieldNames The field values to give the created StatePERAExclusionCodeMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created StatePERAExclusionCodeMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStatePERAExclusionCodeMN <- function(Code = NULL, Description = NULL, MaxAmount = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "StatePERAExclusionCodeMN", body = list(DataObject = body), searchFields = append("StatePERAExclusionCodeMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StatePERAExclusionCodeMN
	#'
	#' This function modifies a StatePERAExclusionCodeMN
	#' @param fieldNames The field values to give the modified StatePERAExclusionCodeMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified StatePERAExclusionCodeMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStatePERAExclusionCodeMN <- function(StatePERAExclusionCodeMNID, Code = NULL, Description = NULL, MaxAmount = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "StatePERAExclusionCodeMN", objectId = StatePERAExclusionCodeMNID, body = list(DataObject = body), searchFields = append("StatePERAExclusionCodeMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StatePERAMemberEmploymentStatusMNS
	#'
	#' This function returns a dataframe or json object of StatePERAMemberEmploymentStatusMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StatePERAMemberEmploymentStatusMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StatePERAMemberEmploymentStatusMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StatePERAMemberEmploymentStatusMN') to get more field paths.
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
	#' @concept Employee
	#' @return A list of StatePERAMemberEmploymentStatusMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStatePERAMemberEmploymentStatusMNS <- function(searchConditionsList = NULL, StatePERAMemberEmploymentStatusMNID = F, Code = F, Description = F, CodeDescription = F, AllowTermination = F, AllowEmploymentStatus = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "StatePERAMemberEmploymentStatusMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StatePERAMemberEmploymentStatusMN
	#'
	#' This function returns a dataframe or json object of a StatePERAMemberEmploymentStatusMN
	#' @param StatePERAMemberEmploymentStatusMNID The ID of the StatePERAMemberEmploymentStatusMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StatePERAMemberEmploymentStatusMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StatePERAMemberEmploymentStatusMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StatePERAMemberEmploymentStatusMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of StatePERAMemberEmploymentStatusMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStatePERAMemberEmploymentStatusMN <- function(StatePERAMemberEmploymentStatusMNID, Code = F, Description = F, CodeDescription = F, AllowTermination = F, AllowEmploymentStatus = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StatePERAMemberEmploymentStatusMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "StatePERAMemberEmploymentStatusMN", objectId = StatePERAMemberEmploymentStatusMNID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StatePERAMemberEmploymentStatusMN
	#'
	#' This function deletes a StatePERAMemberEmploymentStatusMN
	#' @param StatePERAMemberEmploymentStatusMNID The ID of the StatePERAMemberEmploymentStatusMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The StatePERAMemberEmploymentStatusMNID of the deleted StatePERAMemberEmploymentStatusMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStatePERAMemberEmploymentStatusMN <- function(StatePERAMemberEmploymentStatusMNID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "StatePERAMemberEmploymentStatusMN", objectId = StatePERAMemberEmploymentStatusMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StatePERAMemberEmploymentStatusMN
	#'
	#' This function creates a StatePERAMemberEmploymentStatusMN
	#' @param fieldNames The field values to give the created StatePERAMemberEmploymentStatusMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created StatePERAMemberEmploymentStatusMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStatePERAMemberEmploymentStatusMN <- function(Code = NULL, Description = NULL, AllowTermination = NULL, AllowEmploymentStatus = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "StatePERAMemberEmploymentStatusMN", body = list(DataObject = body), searchFields = append("StatePERAMemberEmploymentStatusMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StatePERAMemberEmploymentStatusMN
	#'
	#' This function modifies a StatePERAMemberEmploymentStatusMN
	#' @param fieldNames The field values to give the modified StatePERAMemberEmploymentStatusMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified StatePERAMemberEmploymentStatusMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStatePERAMemberEmploymentStatusMN <- function(StatePERAMemberEmploymentStatusMNID, Code = NULL, Description = NULL, AllowTermination = NULL, AllowEmploymentStatus = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "StatePERAMemberEmploymentStatusMN", objectId = StatePERAMemberEmploymentStatusMNID, body = list(DataObject = body), searchFields = append("StatePERAMemberEmploymentStatusMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Terminations
	#'
	#' This function returns a dataframe or json object of Terminations
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Terminations. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Terminations.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Termination') to get more field paths.
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
	#' @concept Employee
	#' @return A list of Terminations
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTerminations <- function(searchConditionsList = NULL, TerminationID = F, Code = F, Description = F, DistrictID = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "Termination", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Termination
	#'
	#' This function returns a dataframe or json object of a Termination
	#' @param TerminationID The ID of the Termination to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Termination. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Termination.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Termination') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of Termination
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTermination <- function(TerminationID, Code = F, Description = F, DistrictID = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TerminationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "Termination", objectId = TerminationID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Termination
	#'
	#' This function deletes a Termination
	#' @param TerminationID The ID of the Termination to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The TerminationID of the deleted Termination.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTermination <- function(TerminationID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "Termination", objectId = TerminationID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Termination
	#'
	#' This function creates a Termination
	#' @param fieldNames The field values to give the created Termination. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created Termination
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTermination <- function(Code = NULL, Description = NULL, DistrictID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "Termination", body = list(DataObject = body), searchFields = append("TerminationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Termination
	#'
	#' This function modifies a Termination
	#' @param fieldNames The field values to give the modified Termination. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified Termination
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTermination <- function(TerminationID, Code = NULL, Description = NULL, DistrictID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "Termination", objectId = TerminationID, body = list(DataObject = body), searchFields = append("TerminationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EmployeeACAOfferAndCoverages
	#'
	#' This function returns a dataframe or json object of EmployeeACAOfferAndCoverages
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeACAOfferAndCoverages. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeACAOfferAndCoverages.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeACAOfferAndCoverage') to get more field paths.
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
	#' @concept Employee
	#' @return A list of EmployeeACAOfferAndCoverages
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEmployeeACAOfferAndCoverages <- function(searchConditionsList = NULL, EmployeeACAOfferAndCoverageID = F, DistrictID = F, EmployeeID = F, CalendarYear = F, Month = F, FederalACAOfferAndCoverageID = F, FederalACASafeHarborID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ACAEmployeeRequiredContribution = F, EmployerOfferedSelfInsuredCoverage = F, ReportEmployeeAsCoveredIndividual = F, Comment = F, IncludeInACA = F, ACAThirdPartyImportID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "EmployeeACAOfferAndCoverage", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EmployeeACAOfferAndCoverage
	#'
	#' This function returns a dataframe or json object of an EmployeeACAOfferAndCoverage
	#' @param EmployeeACAOfferAndCoverageID The ID of the EmployeeACAOfferAndCoverage to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeACAOfferAndCoverage. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeACAOfferAndCoverage.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeACAOfferAndCoverage') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of EmployeeACAOfferAndCoverage
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEmployeeACAOfferAndCoverage <- function(EmployeeACAOfferAndCoverageID, DistrictID = F, EmployeeID = F, CalendarYear = F, Month = F, FederalACAOfferAndCoverageID = F, FederalACASafeHarborID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ACAEmployeeRequiredContribution = F, EmployerOfferedSelfInsuredCoverage = F, ReportEmployeeAsCoveredIndividual = F, Comment = F, IncludeInACA = F, ACAThirdPartyImportID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EmployeeACAOfferAndCoverageID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "EmployeeACAOfferAndCoverage", objectId = EmployeeACAOfferAndCoverageID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EmployeeACAOfferAndCoverage
	#'
	#' This function deletes an EmployeeACAOfferAndCoverage
	#' @param EmployeeACAOfferAndCoverageID The ID of the EmployeeACAOfferAndCoverage to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The EmployeeACAOfferAndCoverageID of the deleted EmployeeACAOfferAndCoverage.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEmployeeACAOfferAndCoverage <- function(EmployeeACAOfferAndCoverageID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "EmployeeACAOfferAndCoverage", objectId = EmployeeACAOfferAndCoverageID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EmployeeACAOfferAndCoverage
	#'
	#' This function creates an EmployeeACAOfferAndCoverage
	#' @param fieldNames The field values to give the created EmployeeACAOfferAndCoverage. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created EmployeeACAOfferAndCoverage
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEmployeeACAOfferAndCoverage <- function(DistrictID = NULL, EmployeeID = NULL, CalendarYear = NULL, Month = NULL, FederalACAOfferAndCoverageID = NULL, FederalACASafeHarborID = NULL, ACAEmployeeRequiredContribution = NULL, EmployerOfferedSelfInsuredCoverage = NULL, ReportEmployeeAsCoveredIndividual = NULL, Comment = NULL, IncludeInACA = NULL, ACAThirdPartyImportID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "EmployeeACAOfferAndCoverage", body = list(DataObject = body), searchFields = append("EmployeeACAOfferAndCoverageID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EmployeeACAOfferAndCoverage
	#'
	#' This function modifies an EmployeeACAOfferAndCoverage
	#' @param fieldNames The field values to give the modified EmployeeACAOfferAndCoverage. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified EmployeeACAOfferAndCoverage
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEmployeeACAOfferAndCoverage <- function(EmployeeACAOfferAndCoverageID, DistrictID = NULL, EmployeeID = NULL, CalendarYear = NULL, Month = NULL, FederalACAOfferAndCoverageID = NULL, FederalACASafeHarborID = NULL, ACAEmployeeRequiredContribution = NULL, EmployerOfferedSelfInsuredCoverage = NULL, ReportEmployeeAsCoveredIndividual = NULL, Comment = NULL, IncludeInACA = NULL, ACAThirdPartyImportID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "EmployeeACAOfferAndCoverage", objectId = EmployeeACAOfferAndCoverageID, body = list(DataObject = body), searchFields = append("EmployeeACAOfferAndCoverageID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EmployeeACAOfferAndCoverageDependents
	#'
	#' This function returns a dataframe or json object of EmployeeACAOfferAndCoverageDependents
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeACAOfferAndCoverageDependents. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeACAOfferAndCoverageDependents.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeACAOfferAndCoverageDependent') to get more field paths.
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
	#' @concept Employee
	#' @return A list of EmployeeACAOfferAndCoverageDependents
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEmployeeACAOfferAndCoverageDependents <- function(searchConditionsList = NULL, EmployeeACAOfferAndCoverageDependentID = F, EmployeeACAOfferAndCoverageID = F, DependentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ACAThirdPartyImportID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "EmployeeACAOfferAndCoverageDependent", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EmployeeACAOfferAndCoverageDependent
	#'
	#' This function returns a dataframe or json object of an EmployeeACAOfferAndCoverageDependent
	#' @param EmployeeACAOfferAndCoverageDependentID The ID of the EmployeeACAOfferAndCoverageDependent to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeACAOfferAndCoverageDependent. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeACAOfferAndCoverageDependent.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeACAOfferAndCoverageDependent') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of EmployeeACAOfferAndCoverageDependent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEmployeeACAOfferAndCoverageDependent <- function(EmployeeACAOfferAndCoverageDependentID, EmployeeACAOfferAndCoverageID = F, DependentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ACAThirdPartyImportID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EmployeeACAOfferAndCoverageDependentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "EmployeeACAOfferAndCoverageDependent", objectId = EmployeeACAOfferAndCoverageDependentID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EmployeeACAOfferAndCoverageDependent
	#'
	#' This function deletes an EmployeeACAOfferAndCoverageDependent
	#' @param EmployeeACAOfferAndCoverageDependentID The ID of the EmployeeACAOfferAndCoverageDependent to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The EmployeeACAOfferAndCoverageDependentID of the deleted EmployeeACAOfferAndCoverageDependent.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEmployeeACAOfferAndCoverageDependent <- function(EmployeeACAOfferAndCoverageDependentID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "EmployeeACAOfferAndCoverageDependent", objectId = EmployeeACAOfferAndCoverageDependentID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EmployeeACAOfferAndCoverageDependent
	#'
	#' This function creates an EmployeeACAOfferAndCoverageDependent
	#' @param fieldNames The field values to give the created EmployeeACAOfferAndCoverageDependent. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created EmployeeACAOfferAndCoverageDependent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEmployeeACAOfferAndCoverageDependent <- function(EmployeeACAOfferAndCoverageID = NULL, DependentID = NULL, ACAThirdPartyImportID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "EmployeeACAOfferAndCoverageDependent", body = list(DataObject = body), searchFields = append("EmployeeACAOfferAndCoverageDependentID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EmployeeACAOfferAndCoverageDependent
	#'
	#' This function modifies an EmployeeACAOfferAndCoverageDependent
	#' @param fieldNames The field values to give the modified EmployeeACAOfferAndCoverageDependent. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified EmployeeACAOfferAndCoverageDependent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEmployeeACAOfferAndCoverageDependent <- function(EmployeeACAOfferAndCoverageDependentID, EmployeeACAOfferAndCoverageID = NULL, DependentID = NULL, ACAThirdPartyImportID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "EmployeeACAOfferAndCoverageDependent", objectId = EmployeeACAOfferAndCoverageDependentID, body = list(DataObject = body), searchFields = append("EmployeeACAOfferAndCoverageDependentID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EmploymentStatuses
	#'
	#' This function returns a dataframe or json object of EmploymentStatuses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmploymentStatuses. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmploymentStatuses.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmploymentStatus') to get more field paths.
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
	#' @concept Employee
	#' @return A list of EmploymentStatuses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEmploymentStatuses <- function(searchConditionsList = NULL, EmploymentStatusID = F, Code = F, Description = F, DistrictID = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsLeaveOfAbsence = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "EmploymentStatus", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EmploymentStatus
	#'
	#' This function returns a dataframe or json object of an EmploymentStatus
	#' @param EmploymentStatusID The ID of the EmploymentStatus to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmploymentStatus. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmploymentStatus.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmploymentStatus') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of EmploymentStatus
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEmploymentStatus <- function(EmploymentStatusID, Code = F, Description = F, DistrictID = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsLeaveOfAbsence = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EmploymentStatusID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "EmploymentStatus", objectId = EmploymentStatusID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EmploymentStatus
	#'
	#' This function deletes an EmploymentStatus
	#' @param EmploymentStatusID The ID of the EmploymentStatus to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The EmploymentStatusID of the deleted EmploymentStatus.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEmploymentStatus <- function(EmploymentStatusID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "EmploymentStatus", objectId = EmploymentStatusID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EmploymentStatus
	#'
	#' This function creates an EmploymentStatus
	#' @param fieldNames The field values to give the created EmploymentStatus. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created EmploymentStatus
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEmploymentStatus <- function(Code = NULL, Description = NULL, DistrictID = NULL, IsLeaveOfAbsence = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "EmploymentStatus", body = list(DataObject = body), searchFields = append("EmploymentStatusID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EmploymentStatus
	#'
	#' This function modifies an EmploymentStatus
	#' @param fieldNames The field values to give the modified EmploymentStatus. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified EmploymentStatus
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEmploymentStatus <- function(EmploymentStatusID, Code = NULL, Description = NULL, DistrictID = NULL, IsLeaveOfAbsence = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "EmploymentStatus", objectId = EmploymentStatusID, body = list(DataObject = body), searchFields = append("EmploymentStatusID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EmployeeRetirementMNS
	#'
	#' This function returns a dataframe or json object of EmployeeRetirementMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeRetirementMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeRetirementMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeRetirementMN') to get more field paths.
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
	#' @concept Employee
	#' @return A list of EmployeeRetirementMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEmployeeRetirementMNS <- function(searchConditionsList = NULL, EmployeeRetirementMNID = F, DistrictID = F, EmployeeID = F, StartDate = F, EndDate = F, RetirementNumber = F, StateRetirementAssociationTypeMNID = F, StateTRAExemptStatusMNID = F, StateTRAEligibilityMNID = F, StatePERAPositionCodeMNID = F, StatePERAPositionClassMNID = F, StatePERAExclusionCodeMNID = F, StateTRACurrentPositionMNID = F, RetirementAssociation = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeConversionKey = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "EmployeeRetirementMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EmployeeRetirementMN
	#'
	#' This function returns a dataframe or json object of an EmployeeRetirementMN
	#' @param EmployeeRetirementMNID The ID of the EmployeeRetirementMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeRetirementMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeRetirementMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeRetirementMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of EmployeeRetirementMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEmployeeRetirementMN <- function(EmployeeRetirementMNID, DistrictID = F, EmployeeID = F, StartDate = F, EndDate = F, RetirementNumber = F, StateRetirementAssociationTypeMNID = F, StateTRAExemptStatusMNID = F, StateTRAEligibilityMNID = F, StatePERAPositionCodeMNID = F, StatePERAPositionClassMNID = F, StatePERAExclusionCodeMNID = F, StateTRACurrentPositionMNID = F, RetirementAssociation = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeConversionKey = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EmployeeRetirementMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "EmployeeRetirementMN", objectId = EmployeeRetirementMNID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EmployeeRetirementMN
	#'
	#' This function deletes an EmployeeRetirementMN
	#' @param EmployeeRetirementMNID The ID of the EmployeeRetirementMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The EmployeeRetirementMNID of the deleted EmployeeRetirementMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEmployeeRetirementMN <- function(EmployeeRetirementMNID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "EmployeeRetirementMN", objectId = EmployeeRetirementMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EmployeeRetirementMN
	#'
	#' This function creates an EmployeeRetirementMN
	#' @param fieldNames The field values to give the created EmployeeRetirementMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created EmployeeRetirementMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEmployeeRetirementMN <- function(DistrictID = NULL, EmployeeID = NULL, RetirementNumber = NULL, StateRetirementAssociationTypeMNID = NULL, StateTRAExemptStatusMNID = NULL, StateTRAEligibilityMNID = NULL, StatePERAPositionCodeMNID = NULL, StatePERAPositionClassMNID = NULL, StatePERAExclusionCodeMNID = NULL, StateTRACurrentPositionMNID = NULL, RetirementAssociation = NULL, EmployeeConversionKey = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "EmployeeRetirementMN", body = list(DataObject = body), searchFields = append("EmployeeRetirementMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EmployeeRetirementMN
	#'
	#' This function modifies an EmployeeRetirementMN
	#' @param fieldNames The field values to give the modified EmployeeRetirementMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified EmployeeRetirementMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEmployeeRetirementMN <- function(EmployeeRetirementMNID, DistrictID = NULL, EmployeeID = NULL, RetirementNumber = NULL, StateRetirementAssociationTypeMNID = NULL, StateTRAExemptStatusMNID = NULL, StateTRAEligibilityMNID = NULL, StatePERAPositionCodeMNID = NULL, StatePERAPositionClassMNID = NULL, StatePERAExclusionCodeMNID = NULL, StateTRACurrentPositionMNID = NULL, RetirementAssociation = NULL, EmployeeConversionKey = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "EmployeeRetirementMN", objectId = EmployeeRetirementMNID, body = list(DataObject = body), searchFields = append("EmployeeRetirementMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StateRetirementAssociationTypeMNS
	#'
	#' This function returns a dataframe or json object of StateRetirementAssociationTypeMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StateRetirementAssociationTypeMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StateRetirementAssociationTypeMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StateRetirementAssociationTypeMN') to get more field paths.
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
	#' @concept Employee
	#' @return A list of StateRetirementAssociationTypeMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStateRetirementAssociationTypeMNS <- function(searchConditionsList = NULL, StateRetirementAssociationTypeMNID = F, Code = F, Description = F, IsDCPQualified = F, IsValidForPERAContribution = F, IsValidForPERADemographic = F, Association = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "StateRetirementAssociationTypeMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StateRetirementAssociationTypeMN
	#'
	#' This function returns a dataframe or json object of a StateRetirementAssociationTypeMN
	#' @param StateRetirementAssociationTypeMNID The ID of the StateRetirementAssociationTypeMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StateRetirementAssociationTypeMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StateRetirementAssociationTypeMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StateRetirementAssociationTypeMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of StateRetirementAssociationTypeMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStateRetirementAssociationTypeMN <- function(StateRetirementAssociationTypeMNID, Code = F, Description = F, IsDCPQualified = F, IsValidForPERAContribution = F, IsValidForPERADemographic = F, Association = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StateRetirementAssociationTypeMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "StateRetirementAssociationTypeMN", objectId = StateRetirementAssociationTypeMNID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StateRetirementAssociationTypeMN
	#'
	#' This function deletes a StateRetirementAssociationTypeMN
	#' @param StateRetirementAssociationTypeMNID The ID of the StateRetirementAssociationTypeMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The StateRetirementAssociationTypeMNID of the deleted StateRetirementAssociationTypeMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStateRetirementAssociationTypeMN <- function(StateRetirementAssociationTypeMNID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "StateRetirementAssociationTypeMN", objectId = StateRetirementAssociationTypeMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StateRetirementAssociationTypeMN
	#'
	#' This function creates a StateRetirementAssociationTypeMN
	#' @param fieldNames The field values to give the created StateRetirementAssociationTypeMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created StateRetirementAssociationTypeMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStateRetirementAssociationTypeMN <- function(Code = NULL, Description = NULL, IsDCPQualified = NULL, Association = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "StateRetirementAssociationTypeMN", body = list(DataObject = body), searchFields = append("StateRetirementAssociationTypeMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StateRetirementAssociationTypeMN
	#'
	#' This function modifies a StateRetirementAssociationTypeMN
	#' @param fieldNames The field values to give the modified StateRetirementAssociationTypeMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified StateRetirementAssociationTypeMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStateRetirementAssociationTypeMN <- function(StateRetirementAssociationTypeMNID, Code = NULL, Description = NULL, IsDCPQualified = NULL, Association = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "StateRetirementAssociationTypeMN", objectId = StateRetirementAssociationTypeMNID, body = list(DataObject = body), searchFields = append("StateRetirementAssociationTypeMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempCalendarEvents
	#'
	#' This function returns a dataframe or json object of TempCalendarEvents
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCalendarEvents. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCalendarEvents.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCalendarEvent') to get more field paths.
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
	#' @concept Employee
	#' @return A list of TempCalendarEvents
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempCalendarEvents <- function(searchConditionsList = NULL, TempCalendarEventID = F, CalendarEventID = F, OriginalCalendarEventDetailID = F, TargetCalendarEventDetailID = F, LastYearDate = F, NextYearDate = F, CalendarEventDescription = F, ProcessNewOption = F, ProcessExistingOption = F, HasExistingNextYearDate = F, HasNoExistingNextYearDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "TempCalendarEvent", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempCalendarEvent
	#'
	#' This function returns a dataframe or json object of a TempCalendarEvent
	#' @param TempCalendarEventID The ID of the TempCalendarEvent to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCalendarEvent. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCalendarEvent.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCalendarEvent') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of TempCalendarEvent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempCalendarEvent <- function(TempCalendarEventID, CalendarEventID = F, OriginalCalendarEventDetailID = F, TargetCalendarEventDetailID = F, LastYearDate = F, NextYearDate = F, CalendarEventDescription = F, ProcessNewOption = F, ProcessExistingOption = F, HasExistingNextYearDate = F, HasNoExistingNextYearDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempCalendarEventID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "TempCalendarEvent", objectId = TempCalendarEventID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempCalendarEvent
	#'
	#' This function deletes a TempCalendarEvent
	#' @param TempCalendarEventID The ID of the TempCalendarEvent to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The TempCalendarEventID of the deleted TempCalendarEvent.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempCalendarEvent <- function(TempCalendarEventID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "TempCalendarEvent", objectId = TempCalendarEventID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempCalendarEvent
	#'
	#' This function creates a TempCalendarEvent
	#' @param fieldNames The field values to give the created TempCalendarEvent. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created TempCalendarEvent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempCalendarEvent <- function(CalendarEventID = NULL, OriginalCalendarEventDetailID = NULL, TargetCalendarEventDetailID = NULL, LastYearDate = NULL, NextYearDate = NULL, CalendarEventDescription = NULL, ProcessNewOption = NULL, ProcessExistingOption = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "TempCalendarEvent", body = list(DataObject = body), searchFields = append("TempCalendarEventID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempCalendarEvent
	#'
	#' This function modifies a TempCalendarEvent
	#' @param fieldNames The field values to give the modified TempCalendarEvent. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified TempCalendarEvent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempCalendarEvent <- function(TempCalendarEventID, CalendarEventID = NULL, OriginalCalendarEventDetailID = NULL, TargetCalendarEventDetailID = NULL, LastYearDate = NULL, NextYearDate = NULL, CalendarEventDescription = NULL, ProcessNewOption = NULL, ProcessExistingOption = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "TempCalendarEvent", objectId = TempCalendarEventID, body = list(DataObject = body), searchFields = append("TempCalendarEventID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EmployeeTempCalendars
	#'
	#' This function returns a dataframe or json object of EmployeeTempCalendars
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeTempCalendars. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeTempCalendars.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeTempCalendar') to get more field paths.
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
	#' @concept Employee
	#' @return A list of EmployeeTempCalendars
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEmployeeTempCalendars <- function(searchConditionsList = NULL, TempCalendarID = F, AffectedPrimaryKey = F, CodeDescription = F, OldStartDate = F, OldEndDate = F, NewStartDate = F, NewEndDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "TempCalendar", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EmployeeTempCalendar
	#'
	#' This function returns a dataframe or json object of an EmployeeTempCalendar
	#' @param EmployeeTempCalendarID The ID of the EmployeeTempCalendar to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeTempCalendar. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeTempCalendar.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeTempCalendar') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of EmployeeTempCalendar
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEmployeeTempCalendar <- function(EmployeeTempCalendarID, TempCalendarID = F, AffectedPrimaryKey = F, CodeDescription = F, OldStartDate = F, OldEndDate = F, NewStartDate = F, NewEndDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EmployeeTempCalendarID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "TempCalendar", objectId = EmployeeTempCalendarID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EmployeeTempCalendar
	#'
	#' This function deletes an EmployeeTempCalendar
	#' @param EmployeeTempCalendarID The ID of the EmployeeTempCalendar to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The EmployeeTempCalendarID of the deleted EmployeeTempCalendar.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEmployeeTempCalendar <- function(EmployeeTempCalendarID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "TempCalendar", objectId = EmployeeTempCalendarID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EmployeeTempCalendar
	#'
	#' This function creates an EmployeeTempCalendar
	#' @param fieldNames The field values to give the created EmployeeTempCalendar. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created EmployeeTempCalendar
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEmployeeTempCalendar <- function(AffectedPrimaryKey = NULL, CodeDescription = NULL, OldStartDate = NULL, OldEndDate = NULL, NewStartDate = NULL, NewEndDate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "TempCalendar", body = list(DataObject = body), searchFields = append("TempCalendarID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EmployeeTempCalendar
	#'
	#' This function modifies an EmployeeTempCalendar
	#' @param fieldNames The field values to give the modified EmployeeTempCalendar. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified EmployeeTempCalendar
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEmployeeTempCalendar <- function(TempCalendarID, AffectedPrimaryKey = NULL, CodeDescription = NULL, OldStartDate = NULL, OldEndDate = NULL, NewStartDate = NULL, NewEndDate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "TempCalendar", objectId = TempCalendarID, body = list(DataObject = body), searchFields = append("TempCalendarID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StateTRAStatusMNS
	#'
	#' This function returns a dataframe or json object of StateTRAStatusMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StateTRAStatusMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StateTRAStatusMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StateTRAStatusMN') to get more field paths.
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
	#' @concept Employee
	#' @return A list of StateTRAStatusMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStateTRAStatusMNS <- function(searchConditionsList = NULL, StateTRAStatusMNID = F, Code = F, Description = F, IsTermination = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "StateTRAStatusMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StateTRAStatusMN
	#'
	#' This function returns a dataframe or json object of a StateTRAStatusMN
	#' @param StateTRAStatusMNID The ID of the StateTRAStatusMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StateTRAStatusMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StateTRAStatusMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StateTRAStatusMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of StateTRAStatusMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStateTRAStatusMN <- function(StateTRAStatusMNID, Code = F, Description = F, IsTermination = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StateTRAStatusMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "StateTRAStatusMN", objectId = StateTRAStatusMNID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StateTRAStatusMN
	#'
	#' This function deletes a StateTRAStatusMN
	#' @param StateTRAStatusMNID The ID of the StateTRAStatusMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The StateTRAStatusMNID of the deleted StateTRAStatusMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStateTRAStatusMN <- function(StateTRAStatusMNID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "StateTRAStatusMN", objectId = StateTRAStatusMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StateTRAStatusMN
	#'
	#' This function creates a StateTRAStatusMN
	#' @param fieldNames The field values to give the created StateTRAStatusMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created StateTRAStatusMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStateTRAStatusMN <- function(Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "StateTRAStatusMN", body = list(DataObject = body), searchFields = append("StateTRAStatusMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StateTRAStatusMN
	#'
	#' This function modifies a StateTRAStatusMN
	#' @param fieldNames The field values to give the modified StateTRAStatusMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified StateTRAStatusMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStateTRAStatusMN <- function(StateTRAStatusMNID, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "StateTRAStatusMN", objectId = StateTRAStatusMNID, body = list(DataObject = body), searchFields = append("StateTRAStatusMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CalendarEventDetails
	#'
	#' This function returns a dataframe or json object of CalendarEventDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CalendarEventDetails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CalendarEventDetails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CalendarEventDetail') to get more field paths.
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
	#' @concept Employee
	#' @return A list of CalendarEventDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCalendarEventDetails <- function(searchConditionsList = NULL, CalendarEventDetailID = F, CalendarEventID = F, Date = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "CalendarEventDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CalendarEventDetail
	#'
	#' This function returns a dataframe or json object of a CalendarEventDetail
	#' @param CalendarEventDetailID The ID of the CalendarEventDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CalendarEventDetail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CalendarEventDetail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CalendarEventDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of CalendarEventDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCalendarEventDetail <- function(CalendarEventDetailID, CalendarEventID = F, Date = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CalendarEventDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "CalendarEventDetail", objectId = CalendarEventDetailID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CalendarEventDetail
	#'
	#' This function deletes a CalendarEventDetail
	#' @param CalendarEventDetailID The ID of the CalendarEventDetail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The CalendarEventDetailID of the deleted CalendarEventDetail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCalendarEventDetail <- function(CalendarEventDetailID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "CalendarEventDetail", objectId = CalendarEventDetailID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CalendarEventDetail
	#'
	#' This function creates a CalendarEventDetail
	#' @param fieldNames The field values to give the created CalendarEventDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created CalendarEventDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCalendarEventDetail <- function(CalendarEventID = NULL, Date = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "CalendarEventDetail", body = list(DataObject = body), searchFields = append("CalendarEventDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CalendarEventDetail
	#'
	#' This function modifies a CalendarEventDetail
	#' @param fieldNames The field values to give the modified CalendarEventDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified CalendarEventDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCalendarEventDetail <- function(CalendarEventDetailID, CalendarEventID = NULL, Date = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "CalendarEventDetail", objectId = CalendarEventDetailID, body = list(DataObject = body), searchFields = append("CalendarEventDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EmployeeCalendarEvents
	#'
	#' This function returns a dataframe or json object of EmployeeCalendarEvents
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeCalendarEvents. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeCalendarEvents.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeCalendarEvent') to get more field paths.
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
	#' @concept Employee
	#' @return A list of EmployeeCalendarEvents
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEmployeeCalendarEvents <- function(searchConditionsList = NULL, CalendarEventID = F, Description = F, DistrictID = F, CurrentFiscalYearDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "CalendarEvent", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EmployeeCalendarEvent
	#'
	#' This function returns a dataframe or json object of an EmployeeCalendarEvent
	#' @param EmployeeCalendarEventID The ID of the EmployeeCalendarEvent to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeCalendarEvent. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeCalendarEvent.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeCalendarEvent') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of EmployeeCalendarEvent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEmployeeCalendarEvent <- function(EmployeeCalendarEventID, CalendarEventID = F, Description = F, DistrictID = F, CurrentFiscalYearDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EmployeeCalendarEventID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "CalendarEvent", objectId = EmployeeCalendarEventID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EmployeeCalendarEvent
	#'
	#' This function deletes an EmployeeCalendarEvent
	#' @param EmployeeCalendarEventID The ID of the EmployeeCalendarEvent to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The EmployeeCalendarEventID of the deleted EmployeeCalendarEvent.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEmployeeCalendarEvent <- function(EmployeeCalendarEventID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "CalendarEvent", objectId = EmployeeCalendarEventID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EmployeeCalendarEvent
	#'
	#' This function creates an EmployeeCalendarEvent
	#' @param fieldNames The field values to give the created EmployeeCalendarEvent. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created EmployeeCalendarEvent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEmployeeCalendarEvent <- function(Description = NULL, DistrictID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "CalendarEvent", body = list(DataObject = body), searchFields = append("CalendarEventID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EmployeeCalendarEvent
	#'
	#' This function modifies an EmployeeCalendarEvent
	#' @param fieldNames The field values to give the modified EmployeeCalendarEvent. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified EmployeeCalendarEvent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEmployeeCalendarEvent <- function(CalendarEventID, Description = NULL, DistrictID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "CalendarEvent", objectId = CalendarEventID, body = list(DataObject = body), searchFields = append("CalendarEventID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EmployeeCalendars
	#'
	#' This function returns a dataframe or json object of EmployeeCalendars
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeCalendars. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeCalendars.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeCalendar') to get more field paths.
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
	#' @concept Employee
	#' @return A list of EmployeeCalendars
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEmployeeCalendars <- function(searchConditionsList = NULL, CalendarID = F, Code = F, Description = F, DistrictID = F, FiscalYearID = F, StartDate = F, EndDate = F, IsMondayWorkday = F, IsTuesdayWorkday = F, IsWednesdayWorkday = F, IsThursdayWorkday = F, IsFridayWorkday = F, IsSaturdayWorkday = F, IsSundayWorkday = F, CodeDescription = F, PaidDayCount = F, WorkdayCount = F, CalendarIDClonedFrom = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, WorkdaysInWeek = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "Calendar", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EmployeeCalendar
	#'
	#' This function returns a dataframe or json object of an EmployeeCalendar
	#' @param EmployeeCalendarID The ID of the EmployeeCalendar to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeCalendar. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeCalendar.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeCalendar') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of EmployeeCalendar
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEmployeeCalendar <- function(EmployeeCalendarID, CalendarID = F, Code = F, Description = F, DistrictID = F, FiscalYearID = F, StartDate = F, EndDate = F, IsMondayWorkday = F, IsTuesdayWorkday = F, IsWednesdayWorkday = F, IsThursdayWorkday = F, IsFridayWorkday = F, IsSaturdayWorkday = F, IsSundayWorkday = F, CodeDescription = F, PaidDayCount = F, WorkdayCount = F, CalendarIDClonedFrom = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, WorkdaysInWeek = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EmployeeCalendarID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "Calendar", objectId = EmployeeCalendarID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EmployeeCalendar
	#'
	#' This function deletes an EmployeeCalendar
	#' @param EmployeeCalendarID The ID of the EmployeeCalendar to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The EmployeeCalendarID of the deleted EmployeeCalendar.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEmployeeCalendar <- function(EmployeeCalendarID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "Calendar", objectId = EmployeeCalendarID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EmployeeCalendar
	#'
	#' This function creates an EmployeeCalendar
	#' @param fieldNames The field values to give the created EmployeeCalendar. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created EmployeeCalendar
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEmployeeCalendar <- function(Code = NULL, Description = NULL, DistrictID = NULL, FiscalYearID = NULL, StartDate = NULL, EndDate = NULL, IsMondayWorkday = NULL, IsTuesdayWorkday = NULL, IsWednesdayWorkday = NULL, IsThursdayWorkday = NULL, IsFridayWorkday = NULL, IsSaturdayWorkday = NULL, IsSundayWorkday = NULL, CalendarIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "Calendar", body = list(DataObject = body), searchFields = append("CalendarID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EmployeeCalendar
	#'
	#' This function modifies an EmployeeCalendar
	#' @param fieldNames The field values to give the modified EmployeeCalendar. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified EmployeeCalendar
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEmployeeCalendar <- function(CalendarID, Code = NULL, Description = NULL, DistrictID = NULL, FiscalYearID = NULL, StartDate = NULL, EndDate = NULL, IsMondayWorkday = NULL, IsTuesdayWorkday = NULL, IsWednesdayWorkday = NULL, IsThursdayWorkday = NULL, IsFridayWorkday = NULL, IsSaturdayWorkday = NULL, IsSundayWorkday = NULL, CalendarIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "Calendar", objectId = CalendarID, body = list(DataObject = body), searchFields = append("CalendarID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EmployeeCalendarDays
	#'
	#' This function returns a dataframe or json object of EmployeeCalendarDays
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeCalendarDays. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeCalendarDays.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeCalendarDay') to get more field paths.
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
	#' @concept Employee
	#' @return A list of EmployeeCalendarDays
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEmployeeCalendarDays <- function(searchConditionsList = NULL, CalendarDayID = F, CalendarID = F, Date = F, IsWorkday = F, IsPaid = F, OverrideType = F, Comment = F, OverridePercent = F, OverrideSeconds = F, IsPaidHoliday = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AdditionalSeconds = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "CalendarDay", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EmployeeCalendarDay
	#'
	#' This function returns a dataframe or json object of an EmployeeCalendarDay
	#' @param EmployeeCalendarDayID The ID of the EmployeeCalendarDay to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeCalendarDay. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeCalendarDay.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeCalendarDay') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of EmployeeCalendarDay
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEmployeeCalendarDay <- function(EmployeeCalendarDayID, CalendarDayID = F, CalendarID = F, Date = F, IsWorkday = F, IsPaid = F, OverrideType = F, Comment = F, OverridePercent = F, OverrideSeconds = F, IsPaidHoliday = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AdditionalSeconds = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EmployeeCalendarDayID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "CalendarDay", objectId = EmployeeCalendarDayID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EmployeeCalendarDay
	#'
	#' This function deletes an EmployeeCalendarDay
	#' @param EmployeeCalendarDayID The ID of the EmployeeCalendarDay to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The EmployeeCalendarDayID of the deleted EmployeeCalendarDay.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEmployeeCalendarDay <- function(EmployeeCalendarDayID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "CalendarDay", objectId = EmployeeCalendarDayID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EmployeeCalendarDay
	#'
	#' This function creates an EmployeeCalendarDay
	#' @param fieldNames The field values to give the created EmployeeCalendarDay. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created EmployeeCalendarDay
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEmployeeCalendarDay <- function(CalendarID = NULL, Date = NULL, IsWorkday = NULL, IsPaid = NULL, OverrideType = NULL, Comment = NULL, OverridePercent = NULL, OverrideSeconds = NULL, AdditionalSeconds = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "CalendarDay", body = list(DataObject = body), searchFields = append("CalendarDayID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EmployeeCalendarDay
	#'
	#' This function modifies an EmployeeCalendarDay
	#' @param fieldNames The field values to give the modified EmployeeCalendarDay. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified EmployeeCalendarDay
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEmployeeCalendarDay <- function(CalendarDayID, CalendarID = NULL, Date = NULL, IsWorkday = NULL, IsPaid = NULL, OverrideType = NULL, Comment = NULL, OverridePercent = NULL, OverrideSeconds = NULL, AdditionalSeconds = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "CalendarDay", objectId = CalendarDayID, body = list(DataObject = body), searchFields = append("CalendarDayID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EmployeeCourses
	#'
	#' This function returns a dataframe or json object of EmployeeCourses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeCourses. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeCourses.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeCourse') to get more field paths.
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
	#' @concept Employee
	#' @return A list of EmployeeCourses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEmployeeCourses <- function(searchConditionsList = NULL, CourseID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "Course", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EmployeeCourse
	#'
	#' This function returns a dataframe or json object of an EmployeeCourse
	#' @param EmployeeCourseID The ID of the EmployeeCourse to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeCourse. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeCourse.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeCourse') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of EmployeeCourse
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEmployeeCourse <- function(EmployeeCourseID, CourseID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EmployeeCourseID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "Course", objectId = EmployeeCourseID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EmployeeCourse
	#'
	#' This function deletes an EmployeeCourse
	#' @param EmployeeCourseID The ID of the EmployeeCourse to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The EmployeeCourseID of the deleted EmployeeCourse.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEmployeeCourse <- function(EmployeeCourseID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "Course", objectId = EmployeeCourseID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EmployeeCourse
	#'
	#' This function creates an EmployeeCourse
	#' @param fieldNames The field values to give the created EmployeeCourse. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created EmployeeCourse
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEmployeeCourse <- function(Code = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "Course", body = list(DataObject = body), searchFields = append("CourseID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EmployeeCourse
	#'
	#' This function modifies an EmployeeCourse
	#' @param fieldNames The field values to give the modified EmployeeCourse. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified EmployeeCourse
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEmployeeCourse <- function(CourseID, Code = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "Course", objectId = CourseID, body = list(DataObject = body), searchFields = append("CourseID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Degrees
	#'
	#' This function returns a dataframe or json object of Degrees
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Degrees. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Degrees.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Degree') to get more field paths.
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
	#' @concept Employee
	#' @return A list of Degrees
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDegrees <- function(searchConditionsList = NULL, DegreeID = F, EmployeeID = F, DegreeTypeID = F, InstitutionID = F, ReceivedDate = F, ApprovedDate = F, Credits = F, GPA = F, AdditionalCredits = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DegreeThirdPartyImportID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "Degree", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Degree
	#'
	#' This function returns a dataframe or json object of a Degree
	#' @param DegreeID The ID of the Degree to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Degree. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Degree.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Degree') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of Degree
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDegree <- function(DegreeID, EmployeeID = F, DegreeTypeID = F, InstitutionID = F, ReceivedDate = F, ApprovedDate = F, Credits = F, GPA = F, AdditionalCredits = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DegreeThirdPartyImportID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DegreeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "Degree", objectId = DegreeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Degree
	#'
	#' This function deletes a Degree
	#' @param DegreeID The ID of the Degree to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The DegreeID of the deleted Degree.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDegree <- function(DegreeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "Degree", objectId = DegreeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Degree
	#'
	#' This function creates a Degree
	#' @param fieldNames The field values to give the created Degree. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created Degree
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDegree <- function(EmployeeID = NULL, DegreeTypeID = NULL, InstitutionID = NULL, ReceivedDate = NULL, ApprovedDate = NULL, Credits = NULL, GPA = NULL, AdditionalCredits = NULL, DegreeThirdPartyImportID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "Degree", body = list(DataObject = body), searchFields = append("DegreeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Degree
	#'
	#' This function modifies a Degree
	#' @param fieldNames The field values to give the modified Degree. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified Degree
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDegree <- function(DegreeID, EmployeeID = NULL, DegreeTypeID = NULL, InstitutionID = NULL, ReceivedDate = NULL, ApprovedDate = NULL, Credits = NULL, GPA = NULL, AdditionalCredits = NULL, DegreeThirdPartyImportID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "Degree", objectId = DegreeID, body = list(DataObject = body), searchFields = append("DegreeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DegreeProgramOfStudies
	#'
	#' This function returns a dataframe or json object of DegreeProgramOfStudies
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DegreeProgramOfStudies. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DegreeProgramOfStudies.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DegreeProgramOfStudy') to get more field paths.
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
	#' @concept Employee
	#' @return A list of DegreeProgramOfStudies
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDegreeProgramOfStudies <- function(searchConditionsList = NULL, DegreeProgramOfStudyID = F, DegreeID = F, ProgramOfStudyID = F, Focus = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Comment = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "DegreeProgramOfStudy", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DegreeProgramOfStudy
	#'
	#' This function returns a dataframe or json object of a DegreeProgramOfStudy
	#' @param DegreeProgramOfStudyID The ID of the DegreeProgramOfStudy to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DegreeProgramOfStudy. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DegreeProgramOfStudy.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DegreeProgramOfStudy') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of DegreeProgramOfStudy
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDegreeProgramOfStudy <- function(DegreeProgramOfStudyID, DegreeID = F, ProgramOfStudyID = F, Focus = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Comment = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DegreeProgramOfStudyID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "DegreeProgramOfStudy", objectId = DegreeProgramOfStudyID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DegreeProgramOfStudy
	#'
	#' This function deletes a DegreeProgramOfStudy
	#' @param DegreeProgramOfStudyID The ID of the DegreeProgramOfStudy to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The DegreeProgramOfStudyID of the deleted DegreeProgramOfStudy.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDegreeProgramOfStudy <- function(DegreeProgramOfStudyID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "DegreeProgramOfStudy", objectId = DegreeProgramOfStudyID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DegreeProgramOfStudy
	#'
	#' This function creates a DegreeProgramOfStudy
	#' @param fieldNames The field values to give the created DegreeProgramOfStudy. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created DegreeProgramOfStudy
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDegreeProgramOfStudy <- function(DegreeID = NULL, ProgramOfStudyID = NULL, Focus = NULL, Comment = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "DegreeProgramOfStudy", body = list(DataObject = body), searchFields = append("DegreeProgramOfStudyID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DegreeProgramOfStudy
	#'
	#' This function modifies a DegreeProgramOfStudy
	#' @param fieldNames The field values to give the modified DegreeProgramOfStudy. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified DegreeProgramOfStudy
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDegreeProgramOfStudy <- function(DegreeProgramOfStudyID, DegreeID = NULL, ProgramOfStudyID = NULL, Focus = NULL, Comment = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "DegreeProgramOfStudy", objectId = DegreeProgramOfStudyID, body = list(DataObject = body), searchFields = append("DegreeProgramOfStudyID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Credits
	#'
	#' This function returns a dataframe or json object of Credits
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Credits. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Credits.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Credit') to get more field paths.
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
	#' @concept Employee
	#' @return A list of Credits
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCredits <- function(searchConditionsList = NULL, CreditID = F, EmployeeID = F, ApprovedDate = F, CompletionDate = F, InstitutionID = F, CourseID = F, Description = F, CreditsAttempted = F, CreditsEarned = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CreditThirdPartyImportID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "Credit", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Credit
	#'
	#' This function returns a dataframe or json object of a Credit
	#' @param CreditID The ID of the Credit to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Credit. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Credit.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Credit') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of Credit
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCredit <- function(CreditID, EmployeeID = F, ApprovedDate = F, CompletionDate = F, InstitutionID = F, CourseID = F, Description = F, CreditsAttempted = F, CreditsEarned = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CreditThirdPartyImportID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CreditID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "Credit", objectId = CreditID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Credit
	#'
	#' This function deletes a Credit
	#' @param CreditID The ID of the Credit to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The CreditID of the deleted Credit.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCredit <- function(CreditID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "Credit", objectId = CreditID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Credit
	#'
	#' This function creates a Credit
	#' @param fieldNames The field values to give the created Credit. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created Credit
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCredit <- function(EmployeeID = NULL, ApprovedDate = NULL, CompletionDate = NULL, InstitutionID = NULL, CourseID = NULL, Description = NULL, CreditsAttempted = NULL, CreditsEarned = NULL, CreditThirdPartyImportID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "Credit", body = list(DataObject = body), searchFields = append("CreditID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Credit
	#'
	#' This function modifies a Credit
	#' @param fieldNames The field values to give the modified Credit. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified Credit
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCredit <- function(CreditID, EmployeeID = NULL, ApprovedDate = NULL, CompletionDate = NULL, InstitutionID = NULL, CourseID = NULL, Description = NULL, CreditsAttempted = NULL, CreditsEarned = NULL, CreditThirdPartyImportID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "Credit", objectId = CreditID, body = list(DataObject = body), searchFields = append("CreditID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EmployeeYearExperiences
	#'
	#' This function returns a dataframe or json object of EmployeeYearExperiences
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeYearExperiences. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeYearExperiences.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeYearExperience') to get more field paths.
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
	#' @concept Employee
	#' @return A list of EmployeeYearExperiences
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEmployeeYearExperiences <- function(searchConditionsList = NULL, EmployeeYearExperienceID = F, DistrictID = F, Amount = F, YearExperienceLabelID = F, EmployeeID = F, FiscalYearID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "EmployeeYearExperience", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EmployeeYearExperience
	#'
	#' This function returns a dataframe or json object of an EmployeeYearExperience
	#' @param EmployeeYearExperienceID The ID of the EmployeeYearExperience to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeYearExperience. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeYearExperience.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeYearExperience') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of EmployeeYearExperience
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEmployeeYearExperience <- function(EmployeeYearExperienceID, DistrictID = F, Amount = F, YearExperienceLabelID = F, EmployeeID = F, FiscalYearID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EmployeeYearExperienceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "EmployeeYearExperience", objectId = EmployeeYearExperienceID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EmployeeYearExperience
	#'
	#' This function deletes an EmployeeYearExperience
	#' @param EmployeeYearExperienceID The ID of the EmployeeYearExperience to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The EmployeeYearExperienceID of the deleted EmployeeYearExperience.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEmployeeYearExperience <- function(EmployeeYearExperienceID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "EmployeeYearExperience", objectId = EmployeeYearExperienceID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EmployeeYearExperience
	#'
	#' This function creates an EmployeeYearExperience
	#' @param fieldNames The field values to give the created EmployeeYearExperience. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created EmployeeYearExperience
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEmployeeYearExperience <- function(DistrictID = NULL, Amount = NULL, YearExperienceLabelID = NULL, EmployeeID = NULL, FiscalYearID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "EmployeeYearExperience", body = list(DataObject = body), searchFields = append("EmployeeYearExperienceID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EmployeeYearExperience
	#'
	#' This function modifies an EmployeeYearExperience
	#' @param fieldNames The field values to give the modified EmployeeYearExperience. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified EmployeeYearExperience
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEmployeeYearExperience <- function(EmployeeYearExperienceID, DistrictID = NULL, Amount = NULL, YearExperienceLabelID = NULL, EmployeeID = NULL, FiscalYearID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "EmployeeYearExperience", objectId = EmployeeYearExperienceID, body = list(DataObject = body), searchFields = append("EmployeeYearExperienceID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EmployeeVeteranStatuses
	#'
	#' This function returns a dataframe or json object of EmployeeVeteranStatuses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeVeteranStatuses. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeVeteranStatuses.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeVeteranStatus') to get more field paths.
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
	#' @concept Employee
	#' @return A list of EmployeeVeteranStatuses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEmployeeVeteranStatuses <- function(searchConditionsList = NULL, EmployeeVeteranStatusID = F, Comment = F, EmployeeID = F, VeteranStatusID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeConversionKey = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "EmployeeVeteranStatus", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EmployeeVeteranStatus
	#'
	#' This function returns a dataframe or json object of an EmployeeVeteranStatus
	#' @param EmployeeVeteranStatusID The ID of the EmployeeVeteranStatus to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeVeteranStatus. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeVeteranStatus.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeVeteranStatus') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of EmployeeVeteranStatus
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEmployeeVeteranStatus <- function(EmployeeVeteranStatusID, Comment = F, EmployeeID = F, VeteranStatusID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeConversionKey = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EmployeeVeteranStatusID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "EmployeeVeteranStatus", objectId = EmployeeVeteranStatusID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EmployeeVeteranStatus
	#'
	#' This function deletes an EmployeeVeteranStatus
	#' @param EmployeeVeteranStatusID The ID of the EmployeeVeteranStatus to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The EmployeeVeteranStatusID of the deleted EmployeeVeteranStatus.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEmployeeVeteranStatus <- function(EmployeeVeteranStatusID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "EmployeeVeteranStatus", objectId = EmployeeVeteranStatusID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EmployeeVeteranStatus
	#'
	#' This function creates an EmployeeVeteranStatus
	#' @param fieldNames The field values to give the created EmployeeVeteranStatus. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created EmployeeVeteranStatus
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEmployeeVeteranStatus <- function(Comment = NULL, EmployeeID = NULL, VeteranStatusID = NULL, EmployeeConversionKey = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "EmployeeVeteranStatus", body = list(DataObject = body), searchFields = append("EmployeeVeteranStatusID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EmployeeVeteranStatus
	#'
	#' This function modifies an EmployeeVeteranStatus
	#' @param fieldNames The field values to give the modified EmployeeVeteranStatus. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified EmployeeVeteranStatus
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEmployeeVeteranStatus <- function(EmployeeVeteranStatusID, Comment = NULL, EmployeeID = NULL, VeteranStatusID = NULL, EmployeeConversionKey = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "EmployeeVeteranStatus", objectId = EmployeeVeteranStatusID, body = list(DataObject = body), searchFields = append("EmployeeVeteranStatusID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ProgramOfStudies
	#'
	#' This function returns a dataframe or json object of ProgramOfStudies
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ProgramOfStudies. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ProgramOfStudies.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ProgramOfStudy') to get more field paths.
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
	#' @concept Employee
	#' @return A list of ProgramOfStudies
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listProgramOfStudies <- function(searchConditionsList = NULL, ProgramOfStudyID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "ProgramOfStudy", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ProgramOfStudy
	#'
	#' This function returns a dataframe or json object of a ProgramOfStudy
	#' @param ProgramOfStudyID The ID of the ProgramOfStudy to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ProgramOfStudy. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ProgramOfStudy.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ProgramOfStudy') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of ProgramOfStudy
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getProgramOfStudy <- function(ProgramOfStudyID, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ProgramOfStudyID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "ProgramOfStudy", objectId = ProgramOfStudyID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ProgramOfStudy
	#'
	#' This function deletes a ProgramOfStudy
	#' @param ProgramOfStudyID The ID of the ProgramOfStudy to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The ProgramOfStudyID of the deleted ProgramOfStudy.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteProgramOfStudy <- function(ProgramOfStudyID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "ProgramOfStudy", objectId = ProgramOfStudyID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ProgramOfStudy
	#'
	#' This function creates a ProgramOfStudy
	#' @param fieldNames The field values to give the created ProgramOfStudy. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created ProgramOfStudy
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createProgramOfStudy <- function(Code = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "ProgramOfStudy", body = list(DataObject = body), searchFields = append("ProgramOfStudyID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ProgramOfStudy
	#'
	#' This function modifies a ProgramOfStudy
	#' @param fieldNames The field values to give the modified ProgramOfStudy. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified ProgramOfStudy
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyProgramOfStudy <- function(ProgramOfStudyID, Code = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "ProgramOfStudy", objectId = ProgramOfStudyID, body = list(DataObject = body), searchFields = append("ProgramOfStudyID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List VeteranStatuses
	#'
	#' This function returns a dataframe or json object of VeteranStatuses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given VeteranStatuses. Defaults to FALSE for all return fields which, for convenience, returns all fields for the VeteranStatuses.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('VeteranStatus') to get more field paths.
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
	#' @concept Employee
	#' @return A list of VeteranStatuses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listVeteranStatuses <- function(searchConditionsList = NULL, VeteranStatusID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "VeteranStatus", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a VeteranStatus
	#'
	#' This function returns a dataframe or json object of a VeteranStatus
	#' @param VeteranStatusID The ID of the VeteranStatus to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given VeteranStatus. Defaults to FALSE for all return fields which, for convenience, returns all fields for the VeteranStatus.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('VeteranStatus') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of VeteranStatus
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getVeteranStatus <- function(VeteranStatusID, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "VeteranStatusID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "VeteranStatus", objectId = VeteranStatusID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a VeteranStatus
	#'
	#' This function deletes a VeteranStatus
	#' @param VeteranStatusID The ID of the VeteranStatus to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The VeteranStatusID of the deleted VeteranStatus.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteVeteranStatus <- function(VeteranStatusID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "VeteranStatus", objectId = VeteranStatusID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a VeteranStatus
	#'
	#' This function creates a VeteranStatus
	#' @param fieldNames The field values to give the created VeteranStatus. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created VeteranStatus
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createVeteranStatus <- function(Code = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "VeteranStatus", body = list(DataObject = body), searchFields = append("VeteranStatusID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a VeteranStatus
	#'
	#' This function modifies a VeteranStatus
	#' @param fieldNames The field values to give the modified VeteranStatus. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified VeteranStatus
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyVeteranStatus <- function(VeteranStatusID, Code = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "VeteranStatus", objectId = VeteranStatusID, body = list(DataObject = body), searchFields = append("VeteranStatusID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CheckLocations
	#'
	#' This function returns a dataframe or json object of CheckLocations
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CheckLocations. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CheckLocations.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CheckLocation') to get more field paths.
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
	#' @concept Employee
	#' @return A list of CheckLocations
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCheckLocations <- function(searchConditionsList = NULL, CheckLocationID = F, Code = F, Description = F, DistrictID = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "CheckLocation", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CheckLocation
	#'
	#' This function returns a dataframe or json object of a CheckLocation
	#' @param CheckLocationID The ID of the CheckLocation to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CheckLocation. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CheckLocation.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CheckLocation') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of CheckLocation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCheckLocation <- function(CheckLocationID, Code = F, Description = F, DistrictID = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CheckLocationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "CheckLocation", objectId = CheckLocationID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CheckLocation
	#'
	#' This function deletes a CheckLocation
	#' @param CheckLocationID The ID of the CheckLocation to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The CheckLocationID of the deleted CheckLocation.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCheckLocation <- function(CheckLocationID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "CheckLocation", objectId = CheckLocationID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CheckLocation
	#'
	#' This function creates a CheckLocation
	#' @param fieldNames The field values to give the created CheckLocation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created CheckLocation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCheckLocation <- function(Code = NULL, Description = NULL, DistrictID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "CheckLocation", body = list(DataObject = body), searchFields = append("CheckLocationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CheckLocation
	#'
	#' This function modifies a CheckLocation
	#' @param fieldNames The field values to give the modified CheckLocation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified CheckLocation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCheckLocation <- function(CheckLocationID, Code = NULL, Description = NULL, DistrictID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "CheckLocation", objectId = CheckLocationID, body = list(DataObject = body), searchFields = append("CheckLocationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EmployeeTempExceptions
	#'
	#' This function returns a dataframe or json object of EmployeeTempExceptions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeTempExceptions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeTempExceptions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeTempException') to get more field paths.
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
	#' @concept Employee
	#' @return A list of EmployeeTempExceptions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEmployeeTempExceptions <- function(searchConditionsList = NULL, TempExceptionID = F, EmployeeName = F, Message = F, LineNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeNumber = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "TempException", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EmployeeTempException
	#'
	#' This function returns a dataframe or json object of an EmployeeTempException
	#' @param EmployeeTempExceptionID The ID of the EmployeeTempException to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeTempException. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeTempException.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeTempException') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of EmployeeTempException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEmployeeTempException <- function(EmployeeTempExceptionID, TempExceptionID = F, EmployeeName = F, Message = F, LineNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeNumber = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EmployeeTempExceptionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "TempException", objectId = EmployeeTempExceptionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EmployeeTempException
	#'
	#' This function deletes an EmployeeTempException
	#' @param EmployeeTempExceptionID The ID of the EmployeeTempException to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The EmployeeTempExceptionID of the deleted EmployeeTempException.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEmployeeTempException <- function(EmployeeTempExceptionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "TempException", objectId = EmployeeTempExceptionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EmployeeTempException
	#'
	#' This function creates an EmployeeTempException
	#' @param fieldNames The field values to give the created EmployeeTempException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created EmployeeTempException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEmployeeTempException <- function(EmployeeName = NULL, Message = NULL, LineNumber = NULL, EmployeeNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "TempException", body = list(DataObject = body), searchFields = append("TempExceptionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EmployeeTempException
	#'
	#' This function modifies an EmployeeTempException
	#' @param fieldNames The field values to give the modified EmployeeTempException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified EmployeeTempException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEmployeeTempException <- function(TempExceptionID, EmployeeName = NULL, Message = NULL, LineNumber = NULL, EmployeeNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "TempException", objectId = TempExceptionID, body = list(DataObject = body), searchFields = append("TempExceptionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CreditThirdPartyFormats
	#'
	#' This function returns a dataframe or json object of CreditThirdPartyFormats
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditThirdPartyFormats. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditThirdPartyFormats.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditThirdPartyFormat') to get more field paths.
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
	#' @concept Employee
	#' @return A list of CreditThirdPartyFormats
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCreditThirdPartyFormats <- function(searchConditionsList = NULL, CreditThirdPartyFormatID = F, SkywardID = F, SkywardIDClonedFrom = F, DistrictID = F, Code = F, Description = F, CodeDescription = F, IsSystemLoaded = F, ImportType = F, EmployeeIdentification = F, DateFormat = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "CreditThirdPartyFormat", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CreditThirdPartyFormat
	#'
	#' This function returns a dataframe or json object of a CreditThirdPartyFormat
	#' @param CreditThirdPartyFormatID The ID of the CreditThirdPartyFormat to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditThirdPartyFormat. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditThirdPartyFormat.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditThirdPartyFormat') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of CreditThirdPartyFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCreditThirdPartyFormat <- function(CreditThirdPartyFormatID, SkywardID = F, SkywardIDClonedFrom = F, DistrictID = F, Code = F, Description = F, CodeDescription = F, IsSystemLoaded = F, ImportType = F, EmployeeIdentification = F, DateFormat = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CreditThirdPartyFormatID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "CreditThirdPartyFormat", objectId = CreditThirdPartyFormatID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CreditThirdPartyFormat
	#'
	#' This function deletes a CreditThirdPartyFormat
	#' @param CreditThirdPartyFormatID The ID of the CreditThirdPartyFormat to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The CreditThirdPartyFormatID of the deleted CreditThirdPartyFormat.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCreditThirdPartyFormat <- function(CreditThirdPartyFormatID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "CreditThirdPartyFormat", objectId = CreditThirdPartyFormatID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CreditThirdPartyFormat
	#'
	#' This function creates a CreditThirdPartyFormat
	#' @param fieldNames The field values to give the created CreditThirdPartyFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created CreditThirdPartyFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCreditThirdPartyFormat <- function(SkywardIDClonedFrom = NULL, DistrictID = NULL, Code = NULL, Description = NULL, ImportType = NULL, EmployeeIdentification = NULL, DateFormat = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "CreditThirdPartyFormat", body = list(DataObject = body), searchFields = append("CreditThirdPartyFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CreditThirdPartyFormat
	#'
	#' This function modifies a CreditThirdPartyFormat
	#' @param fieldNames The field values to give the modified CreditThirdPartyFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified CreditThirdPartyFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCreditThirdPartyFormat <- function(CreditThirdPartyFormatID, SkywardIDClonedFrom = NULL, DistrictID = NULL, Code = NULL, Description = NULL, ImportType = NULL, EmployeeIdentification = NULL, DateFormat = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "CreditThirdPartyFormat", objectId = CreditThirdPartyFormatID, body = list(DataObject = body), searchFields = append("CreditThirdPartyFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempEmployeeRetirementMNS
	#'
	#' This function returns a dataframe or json object of TempEmployeeRetirementMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempEmployeeRetirementMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempEmployeeRetirementMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempEmployeeRetirementMN') to get more field paths.
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
	#' @concept Employee
	#' @return A list of TempEmployeeRetirementMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempEmployeeRetirementMNS <- function(searchConditionsList = NULL, TempEmployeeRetirementMNID = F, EmployeeRetirementMNID = F, EmployeeFullNameFML = F, RetirementAssociationCode = F, StateRetirementAssociationTypeCodeDescription = F, StatePERAPositionCodeDescription = F, StatePERAPositionClassCodeDescription = F, StatePERAExclusionCodeDescription = F, StateTRACurrentPositionCodeDescription = F, StateTRAExemptStatusCodeDescription = F, StateTRAEligibilityCodeDescription = F, RetirementNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeNumber = F, EndDate = F, StateTRAStatusCodeDescription = F, StateTRAStatusTerminationCodeDescription = F, StatePERAMemberEmploymentStatusCodeDescription = F, StatePERAMemberEmploymentStatusTerminationCodeDescription = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "TempEmployeeRetirementMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempEmployeeRetirementMN
	#'
	#' This function returns a dataframe or json object of a TempEmployeeRetirementMN
	#' @param TempEmployeeRetirementMNID The ID of the TempEmployeeRetirementMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempEmployeeRetirementMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempEmployeeRetirementMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempEmployeeRetirementMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of TempEmployeeRetirementMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempEmployeeRetirementMN <- function(TempEmployeeRetirementMNID, EmployeeRetirementMNID = F, EmployeeFullNameFML = F, RetirementAssociationCode = F, StateRetirementAssociationTypeCodeDescription = F, StatePERAPositionCodeDescription = F, StatePERAPositionClassCodeDescription = F, StatePERAExclusionCodeDescription = F, StateTRACurrentPositionCodeDescription = F, StateTRAExemptStatusCodeDescription = F, StateTRAEligibilityCodeDescription = F, RetirementNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeNumber = F, EndDate = F, StateTRAStatusCodeDescription = F, StateTRAStatusTerminationCodeDescription = F, StatePERAMemberEmploymentStatusCodeDescription = F, StatePERAMemberEmploymentStatusTerminationCodeDescription = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempEmployeeRetirementMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "TempEmployeeRetirementMN", objectId = TempEmployeeRetirementMNID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempEmployeeRetirementMN
	#'
	#' This function deletes a TempEmployeeRetirementMN
	#' @param TempEmployeeRetirementMNID The ID of the TempEmployeeRetirementMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The TempEmployeeRetirementMNID of the deleted TempEmployeeRetirementMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempEmployeeRetirementMN <- function(TempEmployeeRetirementMNID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "TempEmployeeRetirementMN", objectId = TempEmployeeRetirementMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempEmployeeRetirementMN
	#'
	#' This function creates a TempEmployeeRetirementMN
	#' @param fieldNames The field values to give the created TempEmployeeRetirementMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created TempEmployeeRetirementMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempEmployeeRetirementMN <- function(EmployeeRetirementMNID = NULL, EmployeeFullNameFML = NULL, RetirementAssociationCode = NULL, StateRetirementAssociationTypeCodeDescription = NULL, StatePERAPositionCodeDescription = NULL, StatePERAPositionClassCodeDescription = NULL, StatePERAExclusionCodeDescription = NULL, StateTRACurrentPositionCodeDescription = NULL, StateTRAExemptStatusCodeDescription = NULL, StateTRAEligibilityCodeDescription = NULL, RetirementNumber = NULL, EmployeeNumber = NULL, EndDate = NULL, StateTRAStatusCodeDescription = NULL, StateTRAStatusTerminationCodeDescription = NULL, StatePERAMemberEmploymentStatusCodeDescription = NULL, StatePERAMemberEmploymentStatusTerminationCodeDescription = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "TempEmployeeRetirementMN", body = list(DataObject = body), searchFields = append("TempEmployeeRetirementMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempEmployeeRetirementMN
	#'
	#' This function modifies a TempEmployeeRetirementMN
	#' @param fieldNames The field values to give the modified TempEmployeeRetirementMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified TempEmployeeRetirementMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempEmployeeRetirementMN <- function(TempEmployeeRetirementMNID, EmployeeRetirementMNID = NULL, EmployeeFullNameFML = NULL, RetirementAssociationCode = NULL, StateRetirementAssociationTypeCodeDescription = NULL, StatePERAPositionCodeDescription = NULL, StatePERAPositionClassCodeDescription = NULL, StatePERAExclusionCodeDescription = NULL, StateTRACurrentPositionCodeDescription = NULL, StateTRAExemptStatusCodeDescription = NULL, StateTRAEligibilityCodeDescription = NULL, RetirementNumber = NULL, EmployeeNumber = NULL, EndDate = NULL, StateTRAStatusCodeDescription = NULL, StateTRAStatusTerminationCodeDescription = NULL, StatePERAMemberEmploymentStatusCodeDescription = NULL, StatePERAMemberEmploymentStatusTerminationCodeDescription = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "TempEmployeeRetirementMN", objectId = TempEmployeeRetirementMNID, body = list(DataObject = body), searchFields = append("TempEmployeeRetirementMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempEmployeeRetirementMNExceptions
	#'
	#' This function returns a dataframe or json object of TempEmployeeRetirementMNExceptions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempEmployeeRetirementMNExceptions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempEmployeeRetirementMNExceptions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempEmployeeRetirementMNException') to get more field paths.
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
	#' @concept Employee
	#' @return A list of TempEmployeeRetirementMNExceptions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempEmployeeRetirementMNExceptions <- function(searchConditionsList = NULL, TempEmployeeRetirementMNExceptionID = F, EmployeeFullNameFML = F, Error = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeNumber = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "TempEmployeeRetirementMNException", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempEmployeeRetirementMNException
	#'
	#' This function returns a dataframe or json object of a TempEmployeeRetirementMNException
	#' @param TempEmployeeRetirementMNExceptionID The ID of the TempEmployeeRetirementMNException to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempEmployeeRetirementMNException. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempEmployeeRetirementMNException.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempEmployeeRetirementMNException') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of TempEmployeeRetirementMNException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempEmployeeRetirementMNException <- function(TempEmployeeRetirementMNExceptionID, EmployeeFullNameFML = F, Error = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeNumber = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempEmployeeRetirementMNExceptionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "TempEmployeeRetirementMNException", objectId = TempEmployeeRetirementMNExceptionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempEmployeeRetirementMNException
	#'
	#' This function deletes a TempEmployeeRetirementMNException
	#' @param TempEmployeeRetirementMNExceptionID The ID of the TempEmployeeRetirementMNException to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The TempEmployeeRetirementMNExceptionID of the deleted TempEmployeeRetirementMNException.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempEmployeeRetirementMNException <- function(TempEmployeeRetirementMNExceptionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "TempEmployeeRetirementMNException", objectId = TempEmployeeRetirementMNExceptionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempEmployeeRetirementMNException
	#'
	#' This function creates a TempEmployeeRetirementMNException
	#' @param fieldNames The field values to give the created TempEmployeeRetirementMNException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created TempEmployeeRetirementMNException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempEmployeeRetirementMNException <- function(EmployeeFullNameFML = NULL, Error = NULL, EmployeeNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "TempEmployeeRetirementMNException", body = list(DataObject = body), searchFields = append("TempEmployeeRetirementMNExceptionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempEmployeeRetirementMNException
	#'
	#' This function modifies a TempEmployeeRetirementMNException
	#' @param fieldNames The field values to give the modified TempEmployeeRetirementMNException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified TempEmployeeRetirementMNException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempEmployeeRetirementMNException <- function(TempEmployeeRetirementMNExceptionID, EmployeeFullNameFML = NULL, Error = NULL, EmployeeNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "TempEmployeeRetirementMNException", objectId = TempEmployeeRetirementMNExceptionID, body = list(DataObject = body), searchFields = append("TempEmployeeRetirementMNExceptionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CreditDelimitedFileFormats
	#'
	#' This function returns a dataframe or json object of CreditDelimitedFileFormats
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditDelimitedFileFormats. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditDelimitedFileFormats.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditDelimitedFileFormat') to get more field paths.
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
	#' @concept Employee
	#' @return A list of CreditDelimitedFileFormats
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCreditDelimitedFileFormats <- function(searchConditionsList = NULL, CreditDelimitedFileFormatID = F, SkywardID = F, CreditThirdPartyFormatID = F, NumberOfHeaderRows = F, DelimiterType = F, DelimiterCharacter = F, EmployeeColumnNumber = F, CompletionDateColumnNumber = F, ApprovedDateColumnNumber = F, InstitutionNameColumnNumber = F, CourseColumnNumber = F, CreditDescriptionColumnNumber = F, CreditsAttemptedColumnNumber = F, CreditsEarnedColumnNumber = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "CreditDelimitedFileFormat", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CreditDelimitedFileFormat
	#'
	#' This function returns a dataframe or json object of a CreditDelimitedFileFormat
	#' @param CreditDelimitedFileFormatID The ID of the CreditDelimitedFileFormat to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditDelimitedFileFormat. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditDelimitedFileFormat.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditDelimitedFileFormat') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of CreditDelimitedFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCreditDelimitedFileFormat <- function(CreditDelimitedFileFormatID, SkywardID = F, CreditThirdPartyFormatID = F, NumberOfHeaderRows = F, DelimiterType = F, DelimiterCharacter = F, EmployeeColumnNumber = F, CompletionDateColumnNumber = F, ApprovedDateColumnNumber = F, InstitutionNameColumnNumber = F, CourseColumnNumber = F, CreditDescriptionColumnNumber = F, CreditsAttemptedColumnNumber = F, CreditsEarnedColumnNumber = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CreditDelimitedFileFormatID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "CreditDelimitedFileFormat", objectId = CreditDelimitedFileFormatID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CreditDelimitedFileFormat
	#'
	#' This function deletes a CreditDelimitedFileFormat
	#' @param CreditDelimitedFileFormatID The ID of the CreditDelimitedFileFormat to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The CreditDelimitedFileFormatID of the deleted CreditDelimitedFileFormat.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCreditDelimitedFileFormat <- function(CreditDelimitedFileFormatID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "CreditDelimitedFileFormat", objectId = CreditDelimitedFileFormatID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CreditDelimitedFileFormat
	#'
	#' This function creates a CreditDelimitedFileFormat
	#' @param fieldNames The field values to give the created CreditDelimitedFileFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created CreditDelimitedFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCreditDelimitedFileFormat <- function(CreditThirdPartyFormatID = NULL, NumberOfHeaderRows = NULL, DelimiterType = NULL, DelimiterCharacter = NULL, EmployeeColumnNumber = NULL, CompletionDateColumnNumber = NULL, ApprovedDateColumnNumber = NULL, InstitutionNameColumnNumber = NULL, CourseColumnNumber = NULL, CreditDescriptionColumnNumber = NULL, CreditsAttemptedColumnNumber = NULL, CreditsEarnedColumnNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "CreditDelimitedFileFormat", body = list(DataObject = body), searchFields = append("CreditDelimitedFileFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CreditDelimitedFileFormat
	#'
	#' This function modifies a CreditDelimitedFileFormat
	#' @param fieldNames The field values to give the modified CreditDelimitedFileFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified CreditDelimitedFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCreditDelimitedFileFormat <- function(CreditDelimitedFileFormatID, CreditThirdPartyFormatID = NULL, NumberOfHeaderRows = NULL, DelimiterType = NULL, DelimiterCharacter = NULL, EmployeeColumnNumber = NULL, CompletionDateColumnNumber = NULL, ApprovedDateColumnNumber = NULL, InstitutionNameColumnNumber = NULL, CourseColumnNumber = NULL, CreditDescriptionColumnNumber = NULL, CreditsAttemptedColumnNumber = NULL, CreditsEarnedColumnNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "CreditDelimitedFileFormat", objectId = CreditDelimitedFileFormatID, body = list(DataObject = body), searchFields = append("CreditDelimitedFileFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CreditThirdPartyImports
	#'
	#' This function returns a dataframe or json object of CreditThirdPartyImports
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditThirdPartyImports. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditThirdPartyImports.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditThirdPartyImport') to get more field paths.
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
	#' @concept Employee
	#' @return A list of CreditThirdPartyImports
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCreditThirdPartyImports <- function(searchConditionsList = NULL, CreditThirdPartyImportID = F, CreditThirdPartyFormatID = F, ImportTime = F, ImportData = F, MediaID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "CreditThirdPartyImport", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CreditThirdPartyImport
	#'
	#' This function returns a dataframe or json object of a CreditThirdPartyImport
	#' @param CreditThirdPartyImportID The ID of the CreditThirdPartyImport to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditThirdPartyImport. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditThirdPartyImport.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditThirdPartyImport') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of CreditThirdPartyImport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCreditThirdPartyImport <- function(CreditThirdPartyImportID, CreditThirdPartyFormatID = F, ImportTime = F, ImportData = F, MediaID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CreditThirdPartyImportID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "CreditThirdPartyImport", objectId = CreditThirdPartyImportID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CreditThirdPartyImport
	#'
	#' This function deletes a CreditThirdPartyImport
	#' @param CreditThirdPartyImportID The ID of the CreditThirdPartyImport to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The CreditThirdPartyImportID of the deleted CreditThirdPartyImport.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCreditThirdPartyImport <- function(CreditThirdPartyImportID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "CreditThirdPartyImport", objectId = CreditThirdPartyImportID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CreditThirdPartyImport
	#'
	#' This function creates a CreditThirdPartyImport
	#' @param fieldNames The field values to give the created CreditThirdPartyImport. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created CreditThirdPartyImport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCreditThirdPartyImport <- function(CreditThirdPartyFormatID = NULL, ImportTime = NULL, MediaID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "CreditThirdPartyImport", body = list(DataObject = body), searchFields = append("CreditThirdPartyImportID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CreditThirdPartyImport
	#'
	#' This function modifies a CreditThirdPartyImport
	#' @param fieldNames The field values to give the modified CreditThirdPartyImport. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified CreditThirdPartyImport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCreditThirdPartyImport <- function(CreditThirdPartyImportID, CreditThirdPartyFormatID = NULL, ImportTime = NULL, MediaID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "CreditThirdPartyImport", objectId = CreditThirdPartyImportID, body = list(DataObject = body), searchFields = append("CreditThirdPartyImportID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CreditThirdPartyFormatCourses
	#'
	#' This function returns a dataframe or json object of CreditThirdPartyFormatCourses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditThirdPartyFormatCourses. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditThirdPartyFormatCourses.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditThirdPartyFormatCourse') to get more field paths.
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
	#' @concept Employee
	#' @return A list of CreditThirdPartyFormatCourses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCreditThirdPartyFormatCourses <- function(searchConditionsList = NULL, CreditThirdPartyFormatCourseID = F, CreditThirdPartyFormatID = F, CourseID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "CreditThirdPartyFormatCourse", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CreditThirdPartyFormatCourse
	#'
	#' This function returns a dataframe or json object of a CreditThirdPartyFormatCourse
	#' @param CreditThirdPartyFormatCourseID The ID of the CreditThirdPartyFormatCourse to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditThirdPartyFormatCourse. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditThirdPartyFormatCourse.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditThirdPartyFormatCourse') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of CreditThirdPartyFormatCourse
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCreditThirdPartyFormatCourse <- function(CreditThirdPartyFormatCourseID, CreditThirdPartyFormatID = F, CourseID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CreditThirdPartyFormatCourseID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "CreditThirdPartyFormatCourse", objectId = CreditThirdPartyFormatCourseID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CreditThirdPartyFormatCourse
	#'
	#' This function deletes a CreditThirdPartyFormatCourse
	#' @param CreditThirdPartyFormatCourseID The ID of the CreditThirdPartyFormatCourse to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The CreditThirdPartyFormatCourseID of the deleted CreditThirdPartyFormatCourse.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCreditThirdPartyFormatCourse <- function(CreditThirdPartyFormatCourseID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "CreditThirdPartyFormatCourse", objectId = CreditThirdPartyFormatCourseID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CreditThirdPartyFormatCourse
	#'
	#' This function creates a CreditThirdPartyFormatCourse
	#' @param fieldNames The field values to give the created CreditThirdPartyFormatCourse. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created CreditThirdPartyFormatCourse
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCreditThirdPartyFormatCourse <- function(CreditThirdPartyFormatID = NULL, CourseID = NULL, ImportValue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "CreditThirdPartyFormatCourse", body = list(DataObject = body), searchFields = append("CreditThirdPartyFormatCourseID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CreditThirdPartyFormatCourse
	#'
	#' This function modifies a CreditThirdPartyFormatCourse
	#' @param fieldNames The field values to give the modified CreditThirdPartyFormatCourse. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified CreditThirdPartyFormatCourse
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCreditThirdPartyFormatCourse <- function(CreditThirdPartyFormatCourseID, CreditThirdPartyFormatID = NULL, CourseID = NULL, ImportValue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "CreditThirdPartyFormatCourse", objectId = CreditThirdPartyFormatCourseID, body = list(DataObject = body), searchFields = append("CreditThirdPartyFormatCourseID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CreditFixedLengthFileFormats
	#'
	#' This function returns a dataframe or json object of CreditFixedLengthFileFormats
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditFixedLengthFileFormats. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditFixedLengthFileFormats.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditFixedLengthFileFormat') to get more field paths.
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
	#' @concept Employee
	#' @return A list of CreditFixedLengthFileFormats
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCreditFixedLengthFileFormats <- function(searchConditionsList = NULL, CreditFixedLengthFileFormatID = F, SkywardID = F, CreditThirdPartyFormatID = F, NumberOfHeaderRows = F, EmployeeStartPosition = F, EmployeeLength = F, CompletionDateStartPosition = F, CompletionDateLength = F, ApprovedDateStartPosition = F, ApprovedDateLength = F, InstitutionNameStartPosition = F, InstitutionNameLength = F, CourseStartPosition = F, CourseLength = F, CreditDescriptionStartPosition = F, CreditDescriptionLength = F, CreditsAttemptedStartPosition = F, CreditsAttemptedLength = F, CreditsEarnedStartPosition = F, CreditsEarnedLength = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "CreditFixedLengthFileFormat", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CreditFixedLengthFileFormat
	#'
	#' This function returns a dataframe or json object of a CreditFixedLengthFileFormat
	#' @param CreditFixedLengthFileFormatID The ID of the CreditFixedLengthFileFormat to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditFixedLengthFileFormat. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditFixedLengthFileFormat.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditFixedLengthFileFormat') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of CreditFixedLengthFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCreditFixedLengthFileFormat <- function(CreditFixedLengthFileFormatID, SkywardID = F, CreditThirdPartyFormatID = F, NumberOfHeaderRows = F, EmployeeStartPosition = F, EmployeeLength = F, CompletionDateStartPosition = F, CompletionDateLength = F, ApprovedDateStartPosition = F, ApprovedDateLength = F, InstitutionNameStartPosition = F, InstitutionNameLength = F, CourseStartPosition = F, CourseLength = F, CreditDescriptionStartPosition = F, CreditDescriptionLength = F, CreditsAttemptedStartPosition = F, CreditsAttemptedLength = F, CreditsEarnedStartPosition = F, CreditsEarnedLength = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CreditFixedLengthFileFormatID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "CreditFixedLengthFileFormat", objectId = CreditFixedLengthFileFormatID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CreditFixedLengthFileFormat
	#'
	#' This function deletes a CreditFixedLengthFileFormat
	#' @param CreditFixedLengthFileFormatID The ID of the CreditFixedLengthFileFormat to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The CreditFixedLengthFileFormatID of the deleted CreditFixedLengthFileFormat.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCreditFixedLengthFileFormat <- function(CreditFixedLengthFileFormatID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "CreditFixedLengthFileFormat", objectId = CreditFixedLengthFileFormatID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CreditFixedLengthFileFormat
	#'
	#' This function creates a CreditFixedLengthFileFormat
	#' @param fieldNames The field values to give the created CreditFixedLengthFileFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created CreditFixedLengthFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCreditFixedLengthFileFormat <- function(CreditThirdPartyFormatID = NULL, NumberOfHeaderRows = NULL, EmployeeStartPosition = NULL, EmployeeLength = NULL, CompletionDateStartPosition = NULL, CompletionDateLength = NULL, ApprovedDateStartPosition = NULL, ApprovedDateLength = NULL, InstitutionNameStartPosition = NULL, InstitutionNameLength = NULL, CourseStartPosition = NULL, CourseLength = NULL, CreditDescriptionStartPosition = NULL, CreditDescriptionLength = NULL, CreditsAttemptedStartPosition = NULL, CreditsAttemptedLength = NULL, CreditsEarnedStartPosition = NULL, CreditsEarnedLength = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "CreditFixedLengthFileFormat", body = list(DataObject = body), searchFields = append("CreditFixedLengthFileFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CreditFixedLengthFileFormat
	#'
	#' This function modifies a CreditFixedLengthFileFormat
	#' @param fieldNames The field values to give the modified CreditFixedLengthFileFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified CreditFixedLengthFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCreditFixedLengthFileFormat <- function(CreditFixedLengthFileFormatID, CreditThirdPartyFormatID = NULL, NumberOfHeaderRows = NULL, EmployeeStartPosition = NULL, EmployeeLength = NULL, CompletionDateStartPosition = NULL, CompletionDateLength = NULL, ApprovedDateStartPosition = NULL, ApprovedDateLength = NULL, InstitutionNameStartPosition = NULL, InstitutionNameLength = NULL, CourseStartPosition = NULL, CourseLength = NULL, CreditDescriptionStartPosition = NULL, CreditDescriptionLength = NULL, CreditsAttemptedStartPosition = NULL, CreditsAttemptedLength = NULL, CreditsEarnedStartPosition = NULL, CreditsEarnedLength = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "CreditFixedLengthFileFormat", objectId = CreditFixedLengthFileFormatID, body = list(DataObject = body), searchFields = append("CreditFixedLengthFileFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DegreeThirdPartyFormats
	#'
	#' This function returns a dataframe or json object of DegreeThirdPartyFormats
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DegreeThirdPartyFormats. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DegreeThirdPartyFormats.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DegreeThirdPartyFormat') to get more field paths.
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
	#' @concept Employee
	#' @return A list of DegreeThirdPartyFormats
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDegreeThirdPartyFormats <- function(searchConditionsList = NULL, DegreeThirdPartyFormatID = F, SkywardID = F, SkywardIDClonedFrom = F, SkywardHash = F, DistrictID = F, Code = F, Description = F, CodeDescription = F, IsSystemLoaded = F, ImportType = F, EmployeeIdentification = F, DateFormat = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "DegreeThirdPartyFormat", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DegreeThirdPartyFormat
	#'
	#' This function returns a dataframe or json object of a DegreeThirdPartyFormat
	#' @param DegreeThirdPartyFormatID The ID of the DegreeThirdPartyFormat to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DegreeThirdPartyFormat. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DegreeThirdPartyFormat.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DegreeThirdPartyFormat') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of DegreeThirdPartyFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDegreeThirdPartyFormat <- function(DegreeThirdPartyFormatID, SkywardID = F, SkywardIDClonedFrom = F, SkywardHash = F, DistrictID = F, Code = F, Description = F, CodeDescription = F, IsSystemLoaded = F, ImportType = F, EmployeeIdentification = F, DateFormat = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DegreeThirdPartyFormatID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "DegreeThirdPartyFormat", objectId = DegreeThirdPartyFormatID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DegreeThirdPartyFormat
	#'
	#' This function deletes a DegreeThirdPartyFormat
	#' @param DegreeThirdPartyFormatID The ID of the DegreeThirdPartyFormat to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The DegreeThirdPartyFormatID of the deleted DegreeThirdPartyFormat.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDegreeThirdPartyFormat <- function(DegreeThirdPartyFormatID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "DegreeThirdPartyFormat", objectId = DegreeThirdPartyFormatID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DegreeThirdPartyFormat
	#'
	#' This function creates a DegreeThirdPartyFormat
	#' @param fieldNames The field values to give the created DegreeThirdPartyFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created DegreeThirdPartyFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDegreeThirdPartyFormat <- function(SkywardIDClonedFrom = NULL, DistrictID = NULL, Code = NULL, Description = NULL, ImportType = NULL, EmployeeIdentification = NULL, DateFormat = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "DegreeThirdPartyFormat", body = list(DataObject = body), searchFields = append("DegreeThirdPartyFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DegreeThirdPartyFormat
	#'
	#' This function modifies a DegreeThirdPartyFormat
	#' @param fieldNames The field values to give the modified DegreeThirdPartyFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified DegreeThirdPartyFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDegreeThirdPartyFormat <- function(DegreeThirdPartyFormatID, SkywardIDClonedFrom = NULL, DistrictID = NULL, Code = NULL, Description = NULL, ImportType = NULL, EmployeeIdentification = NULL, DateFormat = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "DegreeThirdPartyFormat", objectId = DegreeThirdPartyFormatID, body = list(DataObject = body), searchFields = append("DegreeThirdPartyFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CreditThirdPartyFormatInstitutions
	#'
	#' This function returns a dataframe or json object of CreditThirdPartyFormatInstitutions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditThirdPartyFormatInstitutions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditThirdPartyFormatInstitutions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditThirdPartyFormatInstitution') to get more field paths.
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
	#' @concept Employee
	#' @return A list of CreditThirdPartyFormatInstitutions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCreditThirdPartyFormatInstitutions <- function(searchConditionsList = NULL, CreditThirdPartyFormatInstitutionID = F, CreditThirdPartyFormatID = F, InstitutionID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "CreditThirdPartyFormatInstitution", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CreditThirdPartyFormatInstitution
	#'
	#' This function returns a dataframe or json object of a CreditThirdPartyFormatInstitution
	#' @param CreditThirdPartyFormatInstitutionID The ID of the CreditThirdPartyFormatInstitution to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditThirdPartyFormatInstitution. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditThirdPartyFormatInstitution.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditThirdPartyFormatInstitution') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of CreditThirdPartyFormatInstitution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCreditThirdPartyFormatInstitution <- function(CreditThirdPartyFormatInstitutionID, CreditThirdPartyFormatID = F, InstitutionID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CreditThirdPartyFormatInstitutionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "CreditThirdPartyFormatInstitution", objectId = CreditThirdPartyFormatInstitutionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CreditThirdPartyFormatInstitution
	#'
	#' This function deletes a CreditThirdPartyFormatInstitution
	#' @param CreditThirdPartyFormatInstitutionID The ID of the CreditThirdPartyFormatInstitution to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The CreditThirdPartyFormatInstitutionID of the deleted CreditThirdPartyFormatInstitution.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCreditThirdPartyFormatInstitution <- function(CreditThirdPartyFormatInstitutionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "CreditThirdPartyFormatInstitution", objectId = CreditThirdPartyFormatInstitutionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CreditThirdPartyFormatInstitution
	#'
	#' This function creates a CreditThirdPartyFormatInstitution
	#' @param fieldNames The field values to give the created CreditThirdPartyFormatInstitution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created CreditThirdPartyFormatInstitution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCreditThirdPartyFormatInstitution <- function(CreditThirdPartyFormatID = NULL, InstitutionID = NULL, ImportValue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "CreditThirdPartyFormatInstitution", body = list(DataObject = body), searchFields = append("CreditThirdPartyFormatInstitutionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CreditThirdPartyFormatInstitution
	#'
	#' This function modifies a CreditThirdPartyFormatInstitution
	#' @param fieldNames The field values to give the modified CreditThirdPartyFormatInstitution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified CreditThirdPartyFormatInstitution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCreditThirdPartyFormatInstitution <- function(CreditThirdPartyFormatInstitutionID, CreditThirdPartyFormatID = NULL, InstitutionID = NULL, ImportValue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "CreditThirdPartyFormatInstitution", objectId = CreditThirdPartyFormatInstitutionID, body = list(DataObject = body), searchFields = append("CreditThirdPartyFormatInstitutionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DegreeThirdPartyFormatFoci
	#'
	#' This function returns a dataframe or json object of DegreeThirdPartyFormatFoci
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DegreeThirdPartyFormatFoci. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DegreeThirdPartyFormatFoci.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DegreeThirdPartyFormatFocus') to get more field paths.
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
	#' @concept Employee
	#' @return A list of DegreeThirdPartyFormatFoci
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDegreeThirdPartyFormatFoci <- function(searchConditionsList = NULL, DegreeThirdPartyFormatFocusID = F, DegreeThirdPartyFormatID = F, Focus = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "DegreeThirdPartyFormatFocus", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DegreeThirdPartyFormatFocus
	#'
	#' This function returns a dataframe or json object of a DegreeThirdPartyFormatFocus
	#' @param DegreeThirdPartyFormatFocusID The ID of the DegreeThirdPartyFormatFocus to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DegreeThirdPartyFormatFocus. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DegreeThirdPartyFormatFocus.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DegreeThirdPartyFormatFocus') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of DegreeThirdPartyFormatFocus
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDegreeThirdPartyFormatFocus <- function(DegreeThirdPartyFormatFocusID, DegreeThirdPartyFormatID = F, Focus = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DegreeThirdPartyFormatFocusID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "DegreeThirdPartyFormatFocus", objectId = DegreeThirdPartyFormatFocusID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DegreeThirdPartyFormatFocus
	#'
	#' This function deletes a DegreeThirdPartyFormatFocus
	#' @param DegreeThirdPartyFormatFocusID The ID of the DegreeThirdPartyFormatFocus to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The DegreeThirdPartyFormatFocusID of the deleted DegreeThirdPartyFormatFocus.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDegreeThirdPartyFormatFocus <- function(DegreeThirdPartyFormatFocusID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "DegreeThirdPartyFormatFocus", objectId = DegreeThirdPartyFormatFocusID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DegreeThirdPartyFormatFocus
	#'
	#' This function creates a DegreeThirdPartyFormatFocus
	#' @param fieldNames The field values to give the created DegreeThirdPartyFormatFocus. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created DegreeThirdPartyFormatFocus
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDegreeThirdPartyFormatFocus <- function(DegreeThirdPartyFormatID = NULL, Focus = NULL, ImportValue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "DegreeThirdPartyFormatFocus", body = list(DataObject = body), searchFields = append("DegreeThirdPartyFormatFocusID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DegreeThirdPartyFormatFocus
	#'
	#' This function modifies a DegreeThirdPartyFormatFocus
	#' @param fieldNames The field values to give the modified DegreeThirdPartyFormatFocus. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified DegreeThirdPartyFormatFocus
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDegreeThirdPartyFormatFocus <- function(DegreeThirdPartyFormatFocusID, DegreeThirdPartyFormatID = NULL, Focus = NULL, ImportValue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "DegreeThirdPartyFormatFocus", objectId = DegreeThirdPartyFormatFocusID, body = list(DataObject = body), searchFields = append("DegreeThirdPartyFormatFocusID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EmployeeFiscalYears
	#'
	#' This function returns a dataframe or json object of EmployeeFiscalYears
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeFiscalYears. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeFiscalYears.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeFiscalYear') to get more field paths.
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
	#' @concept Employee
	#' @return A list of EmployeeFiscalYears
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEmployeeFiscalYears <- function(searchConditionsList = NULL, EmployeeFiscalYearID = F, EmployeeID = F, FiscalYearID = F, DistrictID = F, EmployeeDistrictID = F, PaidDays = F, Workdays = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeFiscalYearIDClonedFrom = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "EmployeeFiscalYear", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EmployeeFiscalYear
	#'
	#' This function returns a dataframe or json object of an EmployeeFiscalYear
	#' @param EmployeeFiscalYearID The ID of the EmployeeFiscalYear to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeFiscalYear. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeFiscalYear.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeFiscalYear') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of EmployeeFiscalYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEmployeeFiscalYear <- function(EmployeeFiscalYearID, EmployeeID = F, FiscalYearID = F, DistrictID = F, EmployeeDistrictID = F, PaidDays = F, Workdays = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeFiscalYearIDClonedFrom = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EmployeeFiscalYearID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "EmployeeFiscalYear", objectId = EmployeeFiscalYearID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EmployeeFiscalYear
	#'
	#' This function deletes an EmployeeFiscalYear
	#' @param EmployeeFiscalYearID The ID of the EmployeeFiscalYear to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The EmployeeFiscalYearID of the deleted EmployeeFiscalYear.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEmployeeFiscalYear <- function(EmployeeFiscalYearID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "EmployeeFiscalYear", objectId = EmployeeFiscalYearID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EmployeeFiscalYear
	#'
	#' This function creates an EmployeeFiscalYear
	#' @param fieldNames The field values to give the created EmployeeFiscalYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created EmployeeFiscalYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEmployeeFiscalYear <- function(EmployeeID = NULL, FiscalYearID = NULL, DistrictID = NULL, EmployeeDistrictID = NULL, EmployeeFiscalYearIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "EmployeeFiscalYear", body = list(DataObject = body), searchFields = append("EmployeeFiscalYearID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EmployeeFiscalYear
	#'
	#' This function modifies an EmployeeFiscalYear
	#' @param fieldNames The field values to give the modified EmployeeFiscalYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified EmployeeFiscalYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEmployeeFiscalYear <- function(EmployeeFiscalYearID, EmployeeID = NULL, FiscalYearID = NULL, DistrictID = NULL, EmployeeDistrictID = NULL, EmployeeFiscalYearIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "EmployeeFiscalYear", objectId = EmployeeFiscalYearID, body = list(DataObject = body), searchFields = append("EmployeeFiscalYearID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DegreeThirdPartyFormatProgramOfStudies
	#'
	#' This function returns a dataframe or json object of DegreeThirdPartyFormatProgramOfStudies
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DegreeThirdPartyFormatProgramOfStudies. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DegreeThirdPartyFormatProgramOfStudies.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DegreeThirdPartyFormatProgramOfStudy') to get more field paths.
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
	#' @concept Employee
	#' @return A list of DegreeThirdPartyFormatProgramOfStudies
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDegreeThirdPartyFormatProgramOfStudies <- function(searchConditionsList = NULL, DegreeThirdPartyFormatProgramOfStudyID = F, DegreeThirdPartyFormatID = F, ProgramOfStudyID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "DegreeThirdPartyFormatProgramOfStudy", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DegreeThirdPartyFormatProgramOfStudy
	#'
	#' This function returns a dataframe or json object of a DegreeThirdPartyFormatProgramOfStudy
	#' @param DegreeThirdPartyFormatProgramOfStudyID The ID of the DegreeThirdPartyFormatProgramOfStudy to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DegreeThirdPartyFormatProgramOfStudy. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DegreeThirdPartyFormatProgramOfStudy.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DegreeThirdPartyFormatProgramOfStudy') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of DegreeThirdPartyFormatProgramOfStudy
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDegreeThirdPartyFormatProgramOfStudy <- function(DegreeThirdPartyFormatProgramOfStudyID, DegreeThirdPartyFormatID = F, ProgramOfStudyID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DegreeThirdPartyFormatProgramOfStudyID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "DegreeThirdPartyFormatProgramOfStudy", objectId = DegreeThirdPartyFormatProgramOfStudyID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DegreeThirdPartyFormatProgramOfStudy
	#'
	#' This function deletes a DegreeThirdPartyFormatProgramOfStudy
	#' @param DegreeThirdPartyFormatProgramOfStudyID The ID of the DegreeThirdPartyFormatProgramOfStudy to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The DegreeThirdPartyFormatProgramOfStudyID of the deleted DegreeThirdPartyFormatProgramOfStudy.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDegreeThirdPartyFormatProgramOfStudy <- function(DegreeThirdPartyFormatProgramOfStudyID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "DegreeThirdPartyFormatProgramOfStudy", objectId = DegreeThirdPartyFormatProgramOfStudyID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DegreeThirdPartyFormatProgramOfStudy
	#'
	#' This function creates a DegreeThirdPartyFormatProgramOfStudy
	#' @param fieldNames The field values to give the created DegreeThirdPartyFormatProgramOfStudy. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created DegreeThirdPartyFormatProgramOfStudy
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDegreeThirdPartyFormatProgramOfStudy <- function(DegreeThirdPartyFormatID = NULL, ProgramOfStudyID = NULL, ImportValue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "DegreeThirdPartyFormatProgramOfStudy", body = list(DataObject = body), searchFields = append("DegreeThirdPartyFormatProgramOfStudyID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DegreeThirdPartyFormatProgramOfStudy
	#'
	#' This function modifies a DegreeThirdPartyFormatProgramOfStudy
	#' @param fieldNames The field values to give the modified DegreeThirdPartyFormatProgramOfStudy. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified DegreeThirdPartyFormatProgramOfStudy
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDegreeThirdPartyFormatProgramOfStudy <- function(DegreeThirdPartyFormatProgramOfStudyID, DegreeThirdPartyFormatID = NULL, ProgramOfStudyID = NULL, ImportValue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "DegreeThirdPartyFormatProgramOfStudy", objectId = DegreeThirdPartyFormatProgramOfStudyID, body = list(DataObject = body), searchFields = append("DegreeThirdPartyFormatProgramOfStudyID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DegreeThirdPartyImports
	#'
	#' This function returns a dataframe or json object of DegreeThirdPartyImports
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DegreeThirdPartyImports. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DegreeThirdPartyImports.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DegreeThirdPartyImport') to get more field paths.
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
	#' @concept Employee
	#' @return A list of DegreeThirdPartyImports
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDegreeThirdPartyImports <- function(searchConditionsList = NULL, DegreeThirdPartyImportID = F, DegreeThirdPartyFormatID = F, ImportTime = F, ImportData = F, MediaID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "DegreeThirdPartyImport", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DegreeThirdPartyImport
	#'
	#' This function returns a dataframe or json object of a DegreeThirdPartyImport
	#' @param DegreeThirdPartyImportID The ID of the DegreeThirdPartyImport to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DegreeThirdPartyImport. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DegreeThirdPartyImport.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DegreeThirdPartyImport') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of DegreeThirdPartyImport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDegreeThirdPartyImport <- function(DegreeThirdPartyImportID, DegreeThirdPartyFormatID = F, ImportTime = F, ImportData = F, MediaID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DegreeThirdPartyImportID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "DegreeThirdPartyImport", objectId = DegreeThirdPartyImportID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DegreeThirdPartyImport
	#'
	#' This function deletes a DegreeThirdPartyImport
	#' @param DegreeThirdPartyImportID The ID of the DegreeThirdPartyImport to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The DegreeThirdPartyImportID of the deleted DegreeThirdPartyImport.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDegreeThirdPartyImport <- function(DegreeThirdPartyImportID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "DegreeThirdPartyImport", objectId = DegreeThirdPartyImportID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DegreeThirdPartyImport
	#'
	#' This function creates a DegreeThirdPartyImport
	#' @param fieldNames The field values to give the created DegreeThirdPartyImport. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created DegreeThirdPartyImport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDegreeThirdPartyImport <- function(DegreeThirdPartyFormatID = NULL, ImportTime = NULL, MediaID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "DegreeThirdPartyImport", body = list(DataObject = body), searchFields = append("DegreeThirdPartyImportID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DegreeThirdPartyImport
	#'
	#' This function modifies a DegreeThirdPartyImport
	#' @param fieldNames The field values to give the modified DegreeThirdPartyImport. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified DegreeThirdPartyImport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDegreeThirdPartyImport <- function(DegreeThirdPartyImportID, DegreeThirdPartyFormatID = NULL, ImportTime = NULL, MediaID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "DegreeThirdPartyImport", objectId = DegreeThirdPartyImportID, body = list(DataObject = body), searchFields = append("DegreeThirdPartyImportID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempCredits
	#'
	#' This function returns a dataframe or json object of TempCredits
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCredits. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCredits.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCredit') to get more field paths.
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
	#' @concept Employee
	#' @return A list of TempCredits
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempCredits <- function(searchConditionsList = NULL, TempCreditID = F, CreditID = F, EmployeeID = F, EmployeeNameLFM = F, ApprovedDate = F, CompletionDate = F, InstitutionID = F, InstitutionName = F, CourseCodeDescription = F, Description = F, CreditsAttempted = F, CreditsEarned = F, ErrorCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CourseID = F, CourseCode = F, HasErrors = F, EmployeeNumber = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "TempCredit", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempCredit
	#'
	#' This function returns a dataframe or json object of a TempCredit
	#' @param TempCreditID The ID of the TempCredit to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCredit. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCredit.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCredit') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of TempCredit
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempCredit <- function(TempCreditID, CreditID = F, EmployeeID = F, EmployeeNameLFM = F, ApprovedDate = F, CompletionDate = F, InstitutionID = F, InstitutionName = F, CourseCodeDescription = F, Description = F, CreditsAttempted = F, CreditsEarned = F, ErrorCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CourseID = F, CourseCode = F, HasErrors = F, EmployeeNumber = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempCreditID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "TempCredit", objectId = TempCreditID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempCredit
	#'
	#' This function deletes a TempCredit
	#' @param TempCreditID The ID of the TempCredit to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The TempCreditID of the deleted TempCredit.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempCredit <- function(TempCreditID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "TempCredit", objectId = TempCreditID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempCredit
	#'
	#' This function creates a TempCredit
	#' @param fieldNames The field values to give the created TempCredit. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created TempCredit
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempCredit <- function(CreditID = NULL, EmployeeID = NULL, EmployeeNameLFM = NULL, ApprovedDate = NULL, CompletionDate = NULL, InstitutionID = NULL, InstitutionName = NULL, CourseCodeDescription = NULL, Description = NULL, CreditsAttempted = NULL, CreditsEarned = NULL, ErrorCount = NULL, CourseID = NULL, CourseCode = NULL, HasErrors = NULL, EmployeeNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "TempCredit", body = list(DataObject = body), searchFields = append("TempCreditID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempCredit
	#'
	#' This function modifies a TempCredit
	#' @param fieldNames The field values to give the modified TempCredit. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified TempCredit
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempCredit <- function(TempCreditID, CreditID = NULL, EmployeeID = NULL, EmployeeNameLFM = NULL, ApprovedDate = NULL, CompletionDate = NULL, InstitutionID = NULL, InstitutionName = NULL, CourseCodeDescription = NULL, Description = NULL, CreditsAttempted = NULL, CreditsEarned = NULL, ErrorCount = NULL, CourseID = NULL, CourseCode = NULL, HasErrors = NULL, EmployeeNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "TempCredit", objectId = TempCreditID, body = list(DataObject = body), searchFields = append("TempCreditID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempCreditErrorDetails
	#'
	#' This function returns a dataframe or json object of TempCreditErrorDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCreditErrorDetails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCreditErrorDetails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCreditErrorDetail') to get more field paths.
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
	#' @concept Employee
	#' @return A list of TempCreditErrorDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempCreditErrorDetails <- function(searchConditionsList = NULL, TempCreditErrorDetailID = F, CreditID = F, Error = F, ErrorDetail = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TempCreditID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "TempCreditErrorDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempCreditErrorDetail
	#'
	#' This function returns a dataframe or json object of a TempCreditErrorDetail
	#' @param TempCreditErrorDetailID The ID of the TempCreditErrorDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCreditErrorDetail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCreditErrorDetail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCreditErrorDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of TempCreditErrorDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempCreditErrorDetail <- function(TempCreditErrorDetailID, CreditID = F, Error = F, ErrorDetail = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TempCreditID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempCreditErrorDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "TempCreditErrorDetail", objectId = TempCreditErrorDetailID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempCreditErrorDetail
	#'
	#' This function deletes a TempCreditErrorDetail
	#' @param TempCreditErrorDetailID The ID of the TempCreditErrorDetail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The TempCreditErrorDetailID of the deleted TempCreditErrorDetail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempCreditErrorDetail <- function(TempCreditErrorDetailID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "TempCreditErrorDetail", objectId = TempCreditErrorDetailID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempCreditErrorDetail
	#'
	#' This function creates a TempCreditErrorDetail
	#' @param fieldNames The field values to give the created TempCreditErrorDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created TempCreditErrorDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempCreditErrorDetail <- function(CreditID = NULL, Error = NULL, ErrorDetail = NULL, TempCreditID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "TempCreditErrorDetail", body = list(DataObject = body), searchFields = append("TempCreditErrorDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempCreditErrorDetail
	#'
	#' This function modifies a TempCreditErrorDetail
	#' @param fieldNames The field values to give the modified TempCreditErrorDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified TempCreditErrorDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempCreditErrorDetail <- function(TempCreditErrorDetailID, CreditID = NULL, Error = NULL, ErrorDetail = NULL, TempCreditID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "TempCreditErrorDetail", objectId = TempCreditErrorDetailID, body = list(DataObject = body), searchFields = append("TempCreditErrorDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DegreeThirdPartyFormatInstitutions
	#'
	#' This function returns a dataframe or json object of DegreeThirdPartyFormatInstitutions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DegreeThirdPartyFormatInstitutions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DegreeThirdPartyFormatInstitutions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DegreeThirdPartyFormatInstitution') to get more field paths.
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
	#' @concept Employee
	#' @return A list of DegreeThirdPartyFormatInstitutions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDegreeThirdPartyFormatInstitutions <- function(searchConditionsList = NULL, DegreeThirdPartyFormatInstitutionID = F, DegreeThirdPartyFormatID = F, InstitutionID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "DegreeThirdPartyFormatInstitution", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DegreeThirdPartyFormatInstitution
	#'
	#' This function returns a dataframe or json object of a DegreeThirdPartyFormatInstitution
	#' @param DegreeThirdPartyFormatInstitutionID The ID of the DegreeThirdPartyFormatInstitution to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DegreeThirdPartyFormatInstitution. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DegreeThirdPartyFormatInstitution.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DegreeThirdPartyFormatInstitution') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of DegreeThirdPartyFormatInstitution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDegreeThirdPartyFormatInstitution <- function(DegreeThirdPartyFormatInstitutionID, DegreeThirdPartyFormatID = F, InstitutionID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DegreeThirdPartyFormatInstitutionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "DegreeThirdPartyFormatInstitution", objectId = DegreeThirdPartyFormatInstitutionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DegreeThirdPartyFormatInstitution
	#'
	#' This function deletes a DegreeThirdPartyFormatInstitution
	#' @param DegreeThirdPartyFormatInstitutionID The ID of the DegreeThirdPartyFormatInstitution to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The DegreeThirdPartyFormatInstitutionID of the deleted DegreeThirdPartyFormatInstitution.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDegreeThirdPartyFormatInstitution <- function(DegreeThirdPartyFormatInstitutionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "DegreeThirdPartyFormatInstitution", objectId = DegreeThirdPartyFormatInstitutionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DegreeThirdPartyFormatInstitution
	#'
	#' This function creates a DegreeThirdPartyFormatInstitution
	#' @param fieldNames The field values to give the created DegreeThirdPartyFormatInstitution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created DegreeThirdPartyFormatInstitution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDegreeThirdPartyFormatInstitution <- function(DegreeThirdPartyFormatID = NULL, InstitutionID = NULL, ImportValue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "DegreeThirdPartyFormatInstitution", body = list(DataObject = body), searchFields = append("DegreeThirdPartyFormatInstitutionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DegreeThirdPartyFormatInstitution
	#'
	#' This function modifies a DegreeThirdPartyFormatInstitution
	#' @param fieldNames The field values to give the modified DegreeThirdPartyFormatInstitution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified DegreeThirdPartyFormatInstitution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDegreeThirdPartyFormatInstitution <- function(DegreeThirdPartyFormatInstitutionID, DegreeThirdPartyFormatID = NULL, InstitutionID = NULL, ImportValue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "DegreeThirdPartyFormatInstitution", objectId = DegreeThirdPartyFormatInstitutionID, body = list(DataObject = body), searchFields = append("DegreeThirdPartyFormatInstitutionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DegreeDelimitedFileFormats
	#'
	#' This function returns a dataframe or json object of DegreeDelimitedFileFormats
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DegreeDelimitedFileFormats. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DegreeDelimitedFileFormats.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DegreeDelimitedFileFormat') to get more field paths.
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
	#' @concept Employee
	#' @return A list of DegreeDelimitedFileFormats
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDegreeDelimitedFileFormats <- function(searchConditionsList = NULL, DegreeDelimitedFileFormatID = F, SkywardID = F, SkywardHash = F, DegreeThirdPartyFormatID = F, NumberOfHeaderRows = F, DelimiterType = F, DelimiterCharacter = F, EmployeeColumnNumber = F, ReceivedDateColumnNumber = F, ApprovedDateColumnNumber = F, DegreeTypeColumnNumber = F, InstitutionNameColumnNumber = F, GPAColumnNumber = F, CreditsColumnNumber = F, AdditionalCreditsColumnNumber = F, ProgramOfStudyColumnNumber = F, FocusColumnNumber = F, CommentColumnNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "DegreeDelimitedFileFormat", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DegreeDelimitedFileFormat
	#'
	#' This function returns a dataframe or json object of a DegreeDelimitedFileFormat
	#' @param DegreeDelimitedFileFormatID The ID of the DegreeDelimitedFileFormat to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DegreeDelimitedFileFormat. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DegreeDelimitedFileFormat.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DegreeDelimitedFileFormat') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of DegreeDelimitedFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDegreeDelimitedFileFormat <- function(DegreeDelimitedFileFormatID, SkywardID = F, SkywardHash = F, DegreeThirdPartyFormatID = F, NumberOfHeaderRows = F, DelimiterType = F, DelimiterCharacter = F, EmployeeColumnNumber = F, ReceivedDateColumnNumber = F, ApprovedDateColumnNumber = F, DegreeTypeColumnNumber = F, InstitutionNameColumnNumber = F, GPAColumnNumber = F, CreditsColumnNumber = F, AdditionalCreditsColumnNumber = F, ProgramOfStudyColumnNumber = F, FocusColumnNumber = F, CommentColumnNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DegreeDelimitedFileFormatID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "DegreeDelimitedFileFormat", objectId = DegreeDelimitedFileFormatID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DegreeDelimitedFileFormat
	#'
	#' This function deletes a DegreeDelimitedFileFormat
	#' @param DegreeDelimitedFileFormatID The ID of the DegreeDelimitedFileFormat to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The DegreeDelimitedFileFormatID of the deleted DegreeDelimitedFileFormat.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDegreeDelimitedFileFormat <- function(DegreeDelimitedFileFormatID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "DegreeDelimitedFileFormat", objectId = DegreeDelimitedFileFormatID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DegreeDelimitedFileFormat
	#'
	#' This function creates a DegreeDelimitedFileFormat
	#' @param fieldNames The field values to give the created DegreeDelimitedFileFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created DegreeDelimitedFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDegreeDelimitedFileFormat <- function(DegreeThirdPartyFormatID = NULL, NumberOfHeaderRows = NULL, DelimiterType = NULL, DelimiterCharacter = NULL, EmployeeColumnNumber = NULL, ReceivedDateColumnNumber = NULL, ApprovedDateColumnNumber = NULL, DegreeTypeColumnNumber = NULL, InstitutionNameColumnNumber = NULL, GPAColumnNumber = NULL, CreditsColumnNumber = NULL, AdditionalCreditsColumnNumber = NULL, ProgramOfStudyColumnNumber = NULL, FocusColumnNumber = NULL, CommentColumnNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "DegreeDelimitedFileFormat", body = list(DataObject = body), searchFields = append("DegreeDelimitedFileFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DegreeDelimitedFileFormat
	#'
	#' This function modifies a DegreeDelimitedFileFormat
	#' @param fieldNames The field values to give the modified DegreeDelimitedFileFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified DegreeDelimitedFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDegreeDelimitedFileFormat <- function(DegreeDelimitedFileFormatID, DegreeThirdPartyFormatID = NULL, NumberOfHeaderRows = NULL, DelimiterType = NULL, DelimiterCharacter = NULL, EmployeeColumnNumber = NULL, ReceivedDateColumnNumber = NULL, ApprovedDateColumnNumber = NULL, DegreeTypeColumnNumber = NULL, InstitutionNameColumnNumber = NULL, GPAColumnNumber = NULL, CreditsColumnNumber = NULL, AdditionalCreditsColumnNumber = NULL, ProgramOfStudyColumnNumber = NULL, FocusColumnNumber = NULL, CommentColumnNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "DegreeDelimitedFileFormat", objectId = DegreeDelimitedFileFormatID, body = list(DataObject = body), searchFields = append("DegreeDelimitedFileFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DegreeThirdPartyFormatDegreeTypes
	#'
	#' This function returns a dataframe or json object of DegreeThirdPartyFormatDegreeTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DegreeThirdPartyFormatDegreeTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DegreeThirdPartyFormatDegreeTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DegreeThirdPartyFormatDegreeType') to get more field paths.
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
	#' @concept Employee
	#' @return A list of DegreeThirdPartyFormatDegreeTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDegreeThirdPartyFormatDegreeTypes <- function(searchConditionsList = NULL, DegreeThirdPartyFormatDegreeTypeID = F, DegreeThirdPartyFormatID = F, DegreeTypeID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "DegreeThirdPartyFormatDegreeType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DegreeThirdPartyFormatDegreeType
	#'
	#' This function returns a dataframe or json object of a DegreeThirdPartyFormatDegreeType
	#' @param DegreeThirdPartyFormatDegreeTypeID The ID of the DegreeThirdPartyFormatDegreeType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DegreeThirdPartyFormatDegreeType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DegreeThirdPartyFormatDegreeType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DegreeThirdPartyFormatDegreeType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of DegreeThirdPartyFormatDegreeType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDegreeThirdPartyFormatDegreeType <- function(DegreeThirdPartyFormatDegreeTypeID, DegreeThirdPartyFormatID = F, DegreeTypeID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DegreeThirdPartyFormatDegreeTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "DegreeThirdPartyFormatDegreeType", objectId = DegreeThirdPartyFormatDegreeTypeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DegreeThirdPartyFormatDegreeType
	#'
	#' This function deletes a DegreeThirdPartyFormatDegreeType
	#' @param DegreeThirdPartyFormatDegreeTypeID The ID of the DegreeThirdPartyFormatDegreeType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The DegreeThirdPartyFormatDegreeTypeID of the deleted DegreeThirdPartyFormatDegreeType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDegreeThirdPartyFormatDegreeType <- function(DegreeThirdPartyFormatDegreeTypeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "DegreeThirdPartyFormatDegreeType", objectId = DegreeThirdPartyFormatDegreeTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DegreeThirdPartyFormatDegreeType
	#'
	#' This function creates a DegreeThirdPartyFormatDegreeType
	#' @param fieldNames The field values to give the created DegreeThirdPartyFormatDegreeType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created DegreeThirdPartyFormatDegreeType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDegreeThirdPartyFormatDegreeType <- function(DegreeThirdPartyFormatID = NULL, DegreeTypeID = NULL, ImportValue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "DegreeThirdPartyFormatDegreeType", body = list(DataObject = body), searchFields = append("DegreeThirdPartyFormatDegreeTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DegreeThirdPartyFormatDegreeType
	#'
	#' This function modifies a DegreeThirdPartyFormatDegreeType
	#' @param fieldNames The field values to give the modified DegreeThirdPartyFormatDegreeType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified DegreeThirdPartyFormatDegreeType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDegreeThirdPartyFormatDegreeType <- function(DegreeThirdPartyFormatDegreeTypeID, DegreeThirdPartyFormatID = NULL, DegreeTypeID = NULL, ImportValue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "DegreeThirdPartyFormatDegreeType", objectId = DegreeThirdPartyFormatDegreeTypeID, body = list(DataObject = body), searchFields = append("DegreeThirdPartyFormatDegreeTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DegreeFixedLengthFileFormats
	#'
	#' This function returns a dataframe or json object of DegreeFixedLengthFileFormats
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DegreeFixedLengthFileFormats. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DegreeFixedLengthFileFormats.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DegreeFixedLengthFileFormat') to get more field paths.
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
	#' @concept Employee
	#' @return A list of DegreeFixedLengthFileFormats
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDegreeFixedLengthFileFormats <- function(searchConditionsList = NULL, DegreeFixedLengthFileFormatID = F, SkywardID = F, DegreeThirdPartyFormatID = F, NumberOfHeaderRows = F, EmployeeStartPosition = F, EmployeeLength = F, ReceivedDateStartPosition = F, ReceivedDateLength = F, ApprovedDateStartPosition = F, ApprovedDateLength = F, DegreeTypeStartPosition = F, DegreeTypeLength = F, InstitutionNameStartPosition = F, InstitutionNameLength = F, GPAStartPosition = F, GPALength = F, CreditsStartPosition = F, CreditsLength = F, AdditionalCreditsStartPosition = F, AdditionalCreditsLength = F, ProgramOfStudyStartPosition = F, ProgramOfStudyLength = F, FocusStartPosition = F, FocusLength = F, CommentStartPosition = F, CommentLength = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "DegreeFixedLengthFileFormat", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DegreeFixedLengthFileFormat
	#'
	#' This function returns a dataframe or json object of a DegreeFixedLengthFileFormat
	#' @param DegreeFixedLengthFileFormatID The ID of the DegreeFixedLengthFileFormat to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DegreeFixedLengthFileFormat. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DegreeFixedLengthFileFormat.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DegreeFixedLengthFileFormat') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of DegreeFixedLengthFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDegreeFixedLengthFileFormat <- function(DegreeFixedLengthFileFormatID, SkywardID = F, DegreeThirdPartyFormatID = F, NumberOfHeaderRows = F, EmployeeStartPosition = F, EmployeeLength = F, ReceivedDateStartPosition = F, ReceivedDateLength = F, ApprovedDateStartPosition = F, ApprovedDateLength = F, DegreeTypeStartPosition = F, DegreeTypeLength = F, InstitutionNameStartPosition = F, InstitutionNameLength = F, GPAStartPosition = F, GPALength = F, CreditsStartPosition = F, CreditsLength = F, AdditionalCreditsStartPosition = F, AdditionalCreditsLength = F, ProgramOfStudyStartPosition = F, ProgramOfStudyLength = F, FocusStartPosition = F, FocusLength = F, CommentStartPosition = F, CommentLength = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DegreeFixedLengthFileFormatID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "DegreeFixedLengthFileFormat", objectId = DegreeFixedLengthFileFormatID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DegreeFixedLengthFileFormat
	#'
	#' This function deletes a DegreeFixedLengthFileFormat
	#' @param DegreeFixedLengthFileFormatID The ID of the DegreeFixedLengthFileFormat to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The DegreeFixedLengthFileFormatID of the deleted DegreeFixedLengthFileFormat.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDegreeFixedLengthFileFormat <- function(DegreeFixedLengthFileFormatID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "DegreeFixedLengthFileFormat", objectId = DegreeFixedLengthFileFormatID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DegreeFixedLengthFileFormat
	#'
	#' This function creates a DegreeFixedLengthFileFormat
	#' @param fieldNames The field values to give the created DegreeFixedLengthFileFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created DegreeFixedLengthFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDegreeFixedLengthFileFormat <- function(DegreeThirdPartyFormatID = NULL, NumberOfHeaderRows = NULL, EmployeeStartPosition = NULL, EmployeeLength = NULL, ReceivedDateStartPosition = NULL, ReceivedDateLength = NULL, ApprovedDateStartPosition = NULL, ApprovedDateLength = NULL, DegreeTypeStartPosition = NULL, DegreeTypeLength = NULL, InstitutionNameStartPosition = NULL, InstitutionNameLength = NULL, GPAStartPosition = NULL, GPALength = NULL, CreditsStartPosition = NULL, CreditsLength = NULL, AdditionalCreditsStartPosition = NULL, AdditionalCreditsLength = NULL, ProgramOfStudyStartPosition = NULL, ProgramOfStudyLength = NULL, FocusStartPosition = NULL, FocusLength = NULL, CommentStartPosition = NULL, CommentLength = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "DegreeFixedLengthFileFormat", body = list(DataObject = body), searchFields = append("DegreeFixedLengthFileFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DegreeFixedLengthFileFormat
	#'
	#' This function modifies a DegreeFixedLengthFileFormat
	#' @param fieldNames The field values to give the modified DegreeFixedLengthFileFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified DegreeFixedLengthFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDegreeFixedLengthFileFormat <- function(DegreeFixedLengthFileFormatID, DegreeThirdPartyFormatID = NULL, NumberOfHeaderRows = NULL, EmployeeStartPosition = NULL, EmployeeLength = NULL, ReceivedDateStartPosition = NULL, ReceivedDateLength = NULL, ApprovedDateStartPosition = NULL, ApprovedDateLength = NULL, DegreeTypeStartPosition = NULL, DegreeTypeLength = NULL, InstitutionNameStartPosition = NULL, InstitutionNameLength = NULL, GPAStartPosition = NULL, GPALength = NULL, CreditsStartPosition = NULL, CreditsLength = NULL, AdditionalCreditsStartPosition = NULL, AdditionalCreditsLength = NULL, ProgramOfStudyStartPosition = NULL, ProgramOfStudyLength = NULL, FocusStartPosition = NULL, FocusLength = NULL, CommentStartPosition = NULL, CommentLength = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "DegreeFixedLengthFileFormat", objectId = DegreeFixedLengthFileFormatID, body = list(DataObject = body), searchFields = append("DegreeFixedLengthFileFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EmployeeTempDegrees
	#'
	#' This function returns a dataframe or json object of EmployeeTempDegrees
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeTempDegrees. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeTempDegrees.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeTempDegree') to get more field paths.
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
	#' @concept Employee
	#' @return A list of EmployeeTempDegrees
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEmployeeTempDegrees <- function(searchConditionsList = NULL, TempDegreeID = F, DegreeID = F, EmployeeNameLFM = F, ReceivedDate = F, ApprovedDate = F, InstitutionName = F, GPA = F, Credits = F, AdditionalCredits = F, ErrorCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeID = F, DegreeTypeID = F, DegreeTypeCode = F, InstitutionID = F, FocusCodes = F, ProgramOfStudyCodes = F, Comments = F, NumberOfRecords = F, HasErrors = F, LineNumber = F, EmployeeNumber = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "TempDegree", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EmployeeTempDegree
	#'
	#' This function returns a dataframe or json object of an EmployeeTempDegree
	#' @param EmployeeTempDegreeID The ID of the EmployeeTempDegree to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeTempDegree. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeTempDegree.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeTempDegree') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of EmployeeTempDegree
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEmployeeTempDegree <- function(EmployeeTempDegreeID, TempDegreeID = F, DegreeID = F, EmployeeNameLFM = F, ReceivedDate = F, ApprovedDate = F, InstitutionName = F, GPA = F, Credits = F, AdditionalCredits = F, ErrorCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeID = F, DegreeTypeID = F, DegreeTypeCode = F, InstitutionID = F, FocusCodes = F, ProgramOfStudyCodes = F, Comments = F, NumberOfRecords = F, HasErrors = F, LineNumber = F, EmployeeNumber = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EmployeeTempDegreeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "TempDegree", objectId = EmployeeTempDegreeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EmployeeTempDegree
	#'
	#' This function deletes an EmployeeTempDegree
	#' @param EmployeeTempDegreeID The ID of the EmployeeTempDegree to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The EmployeeTempDegreeID of the deleted EmployeeTempDegree.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEmployeeTempDegree <- function(EmployeeTempDegreeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "TempDegree", objectId = EmployeeTempDegreeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EmployeeTempDegree
	#'
	#' This function creates an EmployeeTempDegree
	#' @param fieldNames The field values to give the created EmployeeTempDegree. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created EmployeeTempDegree
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEmployeeTempDegree <- function(DegreeID = NULL, EmployeeNameLFM = NULL, ReceivedDate = NULL, ApprovedDate = NULL, InstitutionName = NULL, GPA = NULL, Credits = NULL, AdditionalCredits = NULL, ErrorCount = NULL, EmployeeID = NULL, DegreeTypeID = NULL, DegreeTypeCode = NULL, InstitutionID = NULL, FocusCodes = NULL, ProgramOfStudyCodes = NULL, Comments = NULL, NumberOfRecords = NULL, HasErrors = NULL, LineNumber = NULL, EmployeeNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "TempDegree", body = list(DataObject = body), searchFields = append("TempDegreeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EmployeeTempDegree
	#'
	#' This function modifies an EmployeeTempDegree
	#' @param fieldNames The field values to give the modified EmployeeTempDegree. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified EmployeeTempDegree
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEmployeeTempDegree <- function(TempDegreeID, DegreeID = NULL, EmployeeNameLFM = NULL, ReceivedDate = NULL, ApprovedDate = NULL, InstitutionName = NULL, GPA = NULL, Credits = NULL, AdditionalCredits = NULL, ErrorCount = NULL, EmployeeID = NULL, DegreeTypeID = NULL, DegreeTypeCode = NULL, InstitutionID = NULL, FocusCodes = NULL, ProgramOfStudyCodes = NULL, Comments = NULL, NumberOfRecords = NULL, HasErrors = NULL, LineNumber = NULL, EmployeeNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "TempDegree", objectId = TempDegreeID, body = list(DataObject = body), searchFields = append("TempDegreeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempDegreeErrorDetails
	#'
	#' This function returns a dataframe or json object of TempDegreeErrorDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempDegreeErrorDetails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempDegreeErrorDetails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempDegreeErrorDetail') to get more field paths.
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
	#' @concept Employee
	#' @return A list of TempDegreeErrorDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempDegreeErrorDetails <- function(searchConditionsList = NULL, TempDegreeErrorDetailID = F, DegreeID = F, Error = F, ErrorDetail = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TempDegreeID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "TempDegreeErrorDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempDegreeErrorDetail
	#'
	#' This function returns a dataframe or json object of a TempDegreeErrorDetail
	#' @param TempDegreeErrorDetailID The ID of the TempDegreeErrorDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempDegreeErrorDetail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempDegreeErrorDetail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempDegreeErrorDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of TempDegreeErrorDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempDegreeErrorDetail <- function(TempDegreeErrorDetailID, DegreeID = F, Error = F, ErrorDetail = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TempDegreeID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempDegreeErrorDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "TempDegreeErrorDetail", objectId = TempDegreeErrorDetailID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempDegreeErrorDetail
	#'
	#' This function deletes a TempDegreeErrorDetail
	#' @param TempDegreeErrorDetailID The ID of the TempDegreeErrorDetail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The TempDegreeErrorDetailID of the deleted TempDegreeErrorDetail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempDegreeErrorDetail <- function(TempDegreeErrorDetailID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "TempDegreeErrorDetail", objectId = TempDegreeErrorDetailID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempDegreeErrorDetail
	#'
	#' This function creates a TempDegreeErrorDetail
	#' @param fieldNames The field values to give the created TempDegreeErrorDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created TempDegreeErrorDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempDegreeErrorDetail <- function(DegreeID = NULL, Error = NULL, ErrorDetail = NULL, TempDegreeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "TempDegreeErrorDetail", body = list(DataObject = body), searchFields = append("TempDegreeErrorDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempDegreeErrorDetail
	#'
	#' This function modifies a TempDegreeErrorDetail
	#' @param fieldNames The field values to give the modified TempDegreeErrorDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified TempDegreeErrorDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempDegreeErrorDetail <- function(TempDegreeErrorDetailID, DegreeID = NULL, Error = NULL, ErrorDetail = NULL, TempDegreeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "TempDegreeErrorDetail", objectId = TempDegreeErrorDetailID, body = list(DataObject = body), searchFields = append("TempDegreeErrorDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EmployeeThirdPartyFormats
	#'
	#' This function returns a dataframe or json object of EmployeeThirdPartyFormats
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeThirdPartyFormats. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeThirdPartyFormats.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeThirdPartyFormat') to get more field paths.
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
	#' @concept Employee
	#' @return A list of EmployeeThirdPartyFormats
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEmployeeThirdPartyFormats <- function(searchConditionsList = NULL, EmployeeThirdPartyFormatID = F, SkywardID = F, SkywardHash = F, SkywardIDClonedFrom = F, DistrictID = F, Code = F, Description = F, CodeDescription = F, IsSystemLoaded = F, ImportType = F, DateFormat = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "EmployeeThirdPartyFormat", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EmployeeThirdPartyFormat
	#'
	#' This function returns a dataframe or json object of an EmployeeThirdPartyFormat
	#' @param EmployeeThirdPartyFormatID The ID of the EmployeeThirdPartyFormat to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeThirdPartyFormat. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeThirdPartyFormat.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeThirdPartyFormat') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of EmployeeThirdPartyFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEmployeeThirdPartyFormat <- function(EmployeeThirdPartyFormatID, SkywardID = F, SkywardHash = F, SkywardIDClonedFrom = F, DistrictID = F, Code = F, Description = F, CodeDescription = F, IsSystemLoaded = F, ImportType = F, DateFormat = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EmployeeThirdPartyFormatID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "EmployeeThirdPartyFormat", objectId = EmployeeThirdPartyFormatID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EmployeeThirdPartyFormat
	#'
	#' This function deletes an EmployeeThirdPartyFormat
	#' @param EmployeeThirdPartyFormatID The ID of the EmployeeThirdPartyFormat to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The EmployeeThirdPartyFormatID of the deleted EmployeeThirdPartyFormat.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEmployeeThirdPartyFormat <- function(EmployeeThirdPartyFormatID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "EmployeeThirdPartyFormat", objectId = EmployeeThirdPartyFormatID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EmployeeThirdPartyFormat
	#'
	#' This function creates an EmployeeThirdPartyFormat
	#' @param fieldNames The field values to give the created EmployeeThirdPartyFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created EmployeeThirdPartyFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEmployeeThirdPartyFormat <- function(SkywardIDClonedFrom = NULL, DistrictID = NULL, Code = NULL, Description = NULL, ImportType = NULL, DateFormat = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "EmployeeThirdPartyFormat", body = list(DataObject = body), searchFields = append("EmployeeThirdPartyFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EmployeeThirdPartyFormat
	#'
	#' This function modifies an EmployeeThirdPartyFormat
	#' @param fieldNames The field values to give the modified EmployeeThirdPartyFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified EmployeeThirdPartyFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEmployeeThirdPartyFormat <- function(EmployeeThirdPartyFormatID, SkywardIDClonedFrom = NULL, DistrictID = NULL, Code = NULL, Description = NULL, ImportType = NULL, DateFormat = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "EmployeeThirdPartyFormat", objectId = EmployeeThirdPartyFormatID, body = list(DataObject = body), searchFields = append("EmployeeThirdPartyFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EmployeeDelimitedFileFormats
	#'
	#' This function returns a dataframe or json object of EmployeeDelimitedFileFormats
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeDelimitedFileFormats. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeDelimitedFileFormats.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeDelimitedFileFormat') to get more field paths.
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
	#' @concept Employee
	#' @return A list of EmployeeDelimitedFileFormats
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEmployeeDelimitedFileFormats <- function(searchConditionsList = NULL, EmployeeDelimitedFileFormatID = F, SkywardID = F, SkywardHash = F, EmployeeThirdPartyFormatID = F, NumberOfHeaderRows = F, DelimiterType = F, DelimiterCharacter = F, NamePrefixColumnNumber = F, LastNameColumnNumber = F, FirstNameColumnNumber = F, MiddleNameColumnNumber = F, NameSuffixColumnNumber = F, SocialSecurityNumberColumnNumber = F, FederalEINColumnNumber = F, BirthDateColumnNumber = F, GenderColumnNumber = F, PhoneNumberColumnNumber = F, PhoneTypeColumnNumber = F, ExtensionColumnNumber = F, EmailAddressColumnNumber = F, EmailTypeColumnNumber = F, W4DateColumnNumber = F, AddressTypeCodeColumnNumber = F, StreetNumberColumnNumber = F, DirectionalCodeColumnNumber = F, StreetNameColumnNumber = F, AddressSecondaryUnitColumnNumber = F, SecondaryUnitNumberColumnNumber = F, AddressLineTwoColumnNumber = F, CityColumnNumber = F, StateColumnNumber = F, ZipCodeColumnNumber = F, CheckLocationColumnNumber = F, EmployeeNumberColumnNumber = F, StartDateColumnNumber = F, HireDateColumnNumber = F, I9DateColumnNumber = F, VendorNumberColumnNumber = F, UsernameColumnNumber = F, AllowEmployeeAccessColumnNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsAlaskanColumnNumber = F, IsAsianColumnNumber = F, IsBlackColumnNumber = F, IsHawaiianColumnNumber = F, IsHispanicColumnNumber = F, IsWhiteColumnNumber = F, ZipCodeAddOnColumnNumber = F, PhoneNumber2ColumnNumber = F, PhoneType2ColumnNumber = F, Extension2ColumnNumber = F, EmailAddress2ColumnNumber = F, EmailType2ColumnNumber = F, PhoneNumber3ColumnNumber = F, PhoneType3ColumnNumber = F, Extension3ColumnNumber = F, EmailAddress3ColumnNumber = F, EmailType3ColumnNumber = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "EmployeeDelimitedFileFormat", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EmployeeDelimitedFileFormat
	#'
	#' This function returns a dataframe or json object of an EmployeeDelimitedFileFormat
	#' @param EmployeeDelimitedFileFormatID The ID of the EmployeeDelimitedFileFormat to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeDelimitedFileFormat. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeDelimitedFileFormat.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeDelimitedFileFormat') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of EmployeeDelimitedFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEmployeeDelimitedFileFormat <- function(EmployeeDelimitedFileFormatID, SkywardID = F, SkywardHash = F, EmployeeThirdPartyFormatID = F, NumberOfHeaderRows = F, DelimiterType = F, DelimiterCharacter = F, NamePrefixColumnNumber = F, LastNameColumnNumber = F, FirstNameColumnNumber = F, MiddleNameColumnNumber = F, NameSuffixColumnNumber = F, SocialSecurityNumberColumnNumber = F, FederalEINColumnNumber = F, BirthDateColumnNumber = F, GenderColumnNumber = F, PhoneNumberColumnNumber = F, PhoneTypeColumnNumber = F, ExtensionColumnNumber = F, EmailAddressColumnNumber = F, EmailTypeColumnNumber = F, W4DateColumnNumber = F, AddressTypeCodeColumnNumber = F, StreetNumberColumnNumber = F, DirectionalCodeColumnNumber = F, StreetNameColumnNumber = F, AddressSecondaryUnitColumnNumber = F, SecondaryUnitNumberColumnNumber = F, AddressLineTwoColumnNumber = F, CityColumnNumber = F, StateColumnNumber = F, ZipCodeColumnNumber = F, CheckLocationColumnNumber = F, EmployeeNumberColumnNumber = F, StartDateColumnNumber = F, HireDateColumnNumber = F, I9DateColumnNumber = F, VendorNumberColumnNumber = F, UsernameColumnNumber = F, AllowEmployeeAccessColumnNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsAlaskanColumnNumber = F, IsAsianColumnNumber = F, IsBlackColumnNumber = F, IsHawaiianColumnNumber = F, IsHispanicColumnNumber = F, IsWhiteColumnNumber = F, ZipCodeAddOnColumnNumber = F, PhoneNumber2ColumnNumber = F, PhoneType2ColumnNumber = F, Extension2ColumnNumber = F, EmailAddress2ColumnNumber = F, EmailType2ColumnNumber = F, PhoneNumber3ColumnNumber = F, PhoneType3ColumnNumber = F, Extension3ColumnNumber = F, EmailAddress3ColumnNumber = F, EmailType3ColumnNumber = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EmployeeDelimitedFileFormatID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "EmployeeDelimitedFileFormat", objectId = EmployeeDelimitedFileFormatID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EmployeeDelimitedFileFormat
	#'
	#' This function deletes an EmployeeDelimitedFileFormat
	#' @param EmployeeDelimitedFileFormatID The ID of the EmployeeDelimitedFileFormat to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The EmployeeDelimitedFileFormatID of the deleted EmployeeDelimitedFileFormat.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEmployeeDelimitedFileFormat <- function(EmployeeDelimitedFileFormatID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "EmployeeDelimitedFileFormat", objectId = EmployeeDelimitedFileFormatID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EmployeeDelimitedFileFormat
	#'
	#' This function creates an EmployeeDelimitedFileFormat
	#' @param fieldNames The field values to give the created EmployeeDelimitedFileFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created EmployeeDelimitedFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEmployeeDelimitedFileFormat <- function(EmployeeThirdPartyFormatID = NULL, NumberOfHeaderRows = NULL, DelimiterType = NULL, DelimiterCharacter = NULL, NamePrefixColumnNumber = NULL, LastNameColumnNumber = NULL, FirstNameColumnNumber = NULL, MiddleNameColumnNumber = NULL, NameSuffixColumnNumber = NULL, SocialSecurityNumberColumnNumber = NULL, FederalEINColumnNumber = NULL, BirthDateColumnNumber = NULL, GenderColumnNumber = NULL, PhoneNumberColumnNumber = NULL, PhoneTypeColumnNumber = NULL, ExtensionColumnNumber = NULL, EmailAddressColumnNumber = NULL, EmailTypeColumnNumber = NULL, W4DateColumnNumber = NULL, AddressTypeCodeColumnNumber = NULL, StreetNumberColumnNumber = NULL, DirectionalCodeColumnNumber = NULL, StreetNameColumnNumber = NULL, AddressSecondaryUnitColumnNumber = NULL, SecondaryUnitNumberColumnNumber = NULL, AddressLineTwoColumnNumber = NULL, CityColumnNumber = NULL, StateColumnNumber = NULL, ZipCodeColumnNumber = NULL, CheckLocationColumnNumber = NULL, EmployeeNumberColumnNumber = NULL, StartDateColumnNumber = NULL, HireDateColumnNumber = NULL, I9DateColumnNumber = NULL, VendorNumberColumnNumber = NULL, UsernameColumnNumber = NULL, AllowEmployeeAccessColumnNumber = NULL, IsAlaskanColumnNumber = NULL, IsAsianColumnNumber = NULL, IsBlackColumnNumber = NULL, IsHawaiianColumnNumber = NULL, IsHispanicColumnNumber = NULL, IsWhiteColumnNumber = NULL, ZipCodeAddOnColumnNumber = NULL, PhoneNumber2ColumnNumber = NULL, PhoneType2ColumnNumber = NULL, Extension2ColumnNumber = NULL, EmailAddress2ColumnNumber = NULL, EmailType2ColumnNumber = NULL, PhoneNumber3ColumnNumber = NULL, PhoneType3ColumnNumber = NULL, Extension3ColumnNumber = NULL, EmailAddress3ColumnNumber = NULL, EmailType3ColumnNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "EmployeeDelimitedFileFormat", body = list(DataObject = body), searchFields = append("EmployeeDelimitedFileFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EmployeeDelimitedFileFormat
	#'
	#' This function modifies an EmployeeDelimitedFileFormat
	#' @param fieldNames The field values to give the modified EmployeeDelimitedFileFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified EmployeeDelimitedFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEmployeeDelimitedFileFormat <- function(EmployeeDelimitedFileFormatID, EmployeeThirdPartyFormatID = NULL, NumberOfHeaderRows = NULL, DelimiterType = NULL, DelimiterCharacter = NULL, NamePrefixColumnNumber = NULL, LastNameColumnNumber = NULL, FirstNameColumnNumber = NULL, MiddleNameColumnNumber = NULL, NameSuffixColumnNumber = NULL, SocialSecurityNumberColumnNumber = NULL, FederalEINColumnNumber = NULL, BirthDateColumnNumber = NULL, GenderColumnNumber = NULL, PhoneNumberColumnNumber = NULL, PhoneTypeColumnNumber = NULL, ExtensionColumnNumber = NULL, EmailAddressColumnNumber = NULL, EmailTypeColumnNumber = NULL, W4DateColumnNumber = NULL, AddressTypeCodeColumnNumber = NULL, StreetNumberColumnNumber = NULL, DirectionalCodeColumnNumber = NULL, StreetNameColumnNumber = NULL, AddressSecondaryUnitColumnNumber = NULL, SecondaryUnitNumberColumnNumber = NULL, AddressLineTwoColumnNumber = NULL, CityColumnNumber = NULL, StateColumnNumber = NULL, ZipCodeColumnNumber = NULL, CheckLocationColumnNumber = NULL, EmployeeNumberColumnNumber = NULL, StartDateColumnNumber = NULL, HireDateColumnNumber = NULL, I9DateColumnNumber = NULL, VendorNumberColumnNumber = NULL, UsernameColumnNumber = NULL, AllowEmployeeAccessColumnNumber = NULL, IsAlaskanColumnNumber = NULL, IsAsianColumnNumber = NULL, IsBlackColumnNumber = NULL, IsHawaiianColumnNumber = NULL, IsHispanicColumnNumber = NULL, IsWhiteColumnNumber = NULL, ZipCodeAddOnColumnNumber = NULL, PhoneNumber2ColumnNumber = NULL, PhoneType2ColumnNumber = NULL, Extension2ColumnNumber = NULL, EmailAddress2ColumnNumber = NULL, EmailType2ColumnNumber = NULL, PhoneNumber3ColumnNumber = NULL, PhoneType3ColumnNumber = NULL, Extension3ColumnNumber = NULL, EmailAddress3ColumnNumber = NULL, EmailType3ColumnNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "EmployeeDelimitedFileFormat", objectId = EmployeeDelimitedFileFormatID, body = list(DataObject = body), searchFields = append("EmployeeDelimitedFileFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EmployeeThirdPartyFormatEmailTypes
	#'
	#' This function returns a dataframe or json object of EmployeeThirdPartyFormatEmailTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeThirdPartyFormatEmailTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeThirdPartyFormatEmailTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeThirdPartyFormatEmailType') to get more field paths.
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
	#' @concept Employee
	#' @return A list of EmployeeThirdPartyFormatEmailTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEmployeeThirdPartyFormatEmailTypes <- function(searchConditionsList = NULL, EmployeeThirdPartyFormatEmailTypeID = F, EmployeeThirdPartyFormatID = F, EmailTypeID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "EmployeeThirdPartyFormatEmailType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EmployeeThirdPartyFormatEmailType
	#'
	#' This function returns a dataframe or json object of an EmployeeThirdPartyFormatEmailType
	#' @param EmployeeThirdPartyFormatEmailTypeID The ID of the EmployeeThirdPartyFormatEmailType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeThirdPartyFormatEmailType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeThirdPartyFormatEmailType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeThirdPartyFormatEmailType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of EmployeeThirdPartyFormatEmailType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEmployeeThirdPartyFormatEmailType <- function(EmployeeThirdPartyFormatEmailTypeID, EmployeeThirdPartyFormatID = F, EmailTypeID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EmployeeThirdPartyFormatEmailTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "EmployeeThirdPartyFormatEmailType", objectId = EmployeeThirdPartyFormatEmailTypeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EmployeeThirdPartyFormatEmailType
	#'
	#' This function deletes an EmployeeThirdPartyFormatEmailType
	#' @param EmployeeThirdPartyFormatEmailTypeID The ID of the EmployeeThirdPartyFormatEmailType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The EmployeeThirdPartyFormatEmailTypeID of the deleted EmployeeThirdPartyFormatEmailType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEmployeeThirdPartyFormatEmailType <- function(EmployeeThirdPartyFormatEmailTypeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "EmployeeThirdPartyFormatEmailType", objectId = EmployeeThirdPartyFormatEmailTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EmployeeThirdPartyFormatEmailType
	#'
	#' This function creates an EmployeeThirdPartyFormatEmailType
	#' @param fieldNames The field values to give the created EmployeeThirdPartyFormatEmailType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created EmployeeThirdPartyFormatEmailType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEmployeeThirdPartyFormatEmailType <- function(EmployeeThirdPartyFormatID = NULL, EmailTypeID = NULL, ImportValue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "EmployeeThirdPartyFormatEmailType", body = list(DataObject = body), searchFields = append("EmployeeThirdPartyFormatEmailTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EmployeeThirdPartyFormatEmailType
	#'
	#' This function modifies an EmployeeThirdPartyFormatEmailType
	#' @param fieldNames The field values to give the modified EmployeeThirdPartyFormatEmailType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified EmployeeThirdPartyFormatEmailType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEmployeeThirdPartyFormatEmailType <- function(EmployeeThirdPartyFormatEmailTypeID, EmployeeThirdPartyFormatID = NULL, EmailTypeID = NULL, ImportValue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "EmployeeThirdPartyFormatEmailType", objectId = EmployeeThirdPartyFormatEmailTypeID, body = list(DataObject = body), searchFields = append("EmployeeThirdPartyFormatEmailTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EmployeeThirdPartyFormatPhoneTypes
	#'
	#' This function returns a dataframe or json object of EmployeeThirdPartyFormatPhoneTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeThirdPartyFormatPhoneTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeThirdPartyFormatPhoneTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeThirdPartyFormatPhoneType') to get more field paths.
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
	#' @concept Employee
	#' @return A list of EmployeeThirdPartyFormatPhoneTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEmployeeThirdPartyFormatPhoneTypes <- function(searchConditionsList = NULL, EmployeeThirdPartyFormatPhoneTypeID = F, EmployeeThirdPartyFormatID = F, PhoneTypeID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "EmployeeThirdPartyFormatPhoneType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EmployeeThirdPartyFormatPhoneType
	#'
	#' This function returns a dataframe or json object of an EmployeeThirdPartyFormatPhoneType
	#' @param EmployeeThirdPartyFormatPhoneTypeID The ID of the EmployeeThirdPartyFormatPhoneType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeThirdPartyFormatPhoneType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeThirdPartyFormatPhoneType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeThirdPartyFormatPhoneType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of EmployeeThirdPartyFormatPhoneType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEmployeeThirdPartyFormatPhoneType <- function(EmployeeThirdPartyFormatPhoneTypeID, EmployeeThirdPartyFormatID = F, PhoneTypeID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EmployeeThirdPartyFormatPhoneTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "EmployeeThirdPartyFormatPhoneType", objectId = EmployeeThirdPartyFormatPhoneTypeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EmployeeThirdPartyFormatPhoneType
	#'
	#' This function deletes an EmployeeThirdPartyFormatPhoneType
	#' @param EmployeeThirdPartyFormatPhoneTypeID The ID of the EmployeeThirdPartyFormatPhoneType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The EmployeeThirdPartyFormatPhoneTypeID of the deleted EmployeeThirdPartyFormatPhoneType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEmployeeThirdPartyFormatPhoneType <- function(EmployeeThirdPartyFormatPhoneTypeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "EmployeeThirdPartyFormatPhoneType", objectId = EmployeeThirdPartyFormatPhoneTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EmployeeThirdPartyFormatPhoneType
	#'
	#' This function creates an EmployeeThirdPartyFormatPhoneType
	#' @param fieldNames The field values to give the created EmployeeThirdPartyFormatPhoneType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created EmployeeThirdPartyFormatPhoneType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEmployeeThirdPartyFormatPhoneType <- function(EmployeeThirdPartyFormatID = NULL, PhoneTypeID = NULL, ImportValue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "EmployeeThirdPartyFormatPhoneType", body = list(DataObject = body), searchFields = append("EmployeeThirdPartyFormatPhoneTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EmployeeThirdPartyFormatPhoneType
	#'
	#' This function modifies an EmployeeThirdPartyFormatPhoneType
	#' @param fieldNames The field values to give the modified EmployeeThirdPartyFormatPhoneType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified EmployeeThirdPartyFormatPhoneType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEmployeeThirdPartyFormatPhoneType <- function(EmployeeThirdPartyFormatPhoneTypeID, EmployeeThirdPartyFormatID = NULL, PhoneTypeID = NULL, ImportValue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "EmployeeThirdPartyFormatPhoneType", objectId = EmployeeThirdPartyFormatPhoneTypeID, body = list(DataObject = body), searchFields = append("EmployeeThirdPartyFormatPhoneTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EmployeeThirdPartyFormatCheckLocations
	#'
	#' This function returns a dataframe or json object of EmployeeThirdPartyFormatCheckLocations
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeThirdPartyFormatCheckLocations. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeThirdPartyFormatCheckLocations.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeThirdPartyFormatCheckLocation') to get more field paths.
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
	#' @concept Employee
	#' @return A list of EmployeeThirdPartyFormatCheckLocations
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEmployeeThirdPartyFormatCheckLocations <- function(searchConditionsList = NULL, EmployeeThirdPartyFormatCheckLocationID = F, EmployeeThirdPartyFormatID = F, CheckLocationID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "EmployeeThirdPartyFormatCheckLocation", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EmployeeThirdPartyFormatCheckLocation
	#'
	#' This function returns a dataframe or json object of an EmployeeThirdPartyFormatCheckLocation
	#' @param EmployeeThirdPartyFormatCheckLocationID The ID of the EmployeeThirdPartyFormatCheckLocation to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeThirdPartyFormatCheckLocation. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeThirdPartyFormatCheckLocation.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeThirdPartyFormatCheckLocation') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of EmployeeThirdPartyFormatCheckLocation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEmployeeThirdPartyFormatCheckLocation <- function(EmployeeThirdPartyFormatCheckLocationID, EmployeeThirdPartyFormatID = F, CheckLocationID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EmployeeThirdPartyFormatCheckLocationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "EmployeeThirdPartyFormatCheckLocation", objectId = EmployeeThirdPartyFormatCheckLocationID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EmployeeThirdPartyFormatCheckLocation
	#'
	#' This function deletes an EmployeeThirdPartyFormatCheckLocation
	#' @param EmployeeThirdPartyFormatCheckLocationID The ID of the EmployeeThirdPartyFormatCheckLocation to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The EmployeeThirdPartyFormatCheckLocationID of the deleted EmployeeThirdPartyFormatCheckLocation.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEmployeeThirdPartyFormatCheckLocation <- function(EmployeeThirdPartyFormatCheckLocationID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "EmployeeThirdPartyFormatCheckLocation", objectId = EmployeeThirdPartyFormatCheckLocationID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EmployeeThirdPartyFormatCheckLocation
	#'
	#' This function creates an EmployeeThirdPartyFormatCheckLocation
	#' @param fieldNames The field values to give the created EmployeeThirdPartyFormatCheckLocation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created EmployeeThirdPartyFormatCheckLocation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEmployeeThirdPartyFormatCheckLocation <- function(EmployeeThirdPartyFormatID = NULL, CheckLocationID = NULL, ImportValue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "EmployeeThirdPartyFormatCheckLocation", body = list(DataObject = body), searchFields = append("EmployeeThirdPartyFormatCheckLocationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EmployeeThirdPartyFormatCheckLocation
	#'
	#' This function modifies an EmployeeThirdPartyFormatCheckLocation
	#' @param fieldNames The field values to give the modified EmployeeThirdPartyFormatCheckLocation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified EmployeeThirdPartyFormatCheckLocation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEmployeeThirdPartyFormatCheckLocation <- function(EmployeeThirdPartyFormatCheckLocationID, EmployeeThirdPartyFormatID = NULL, CheckLocationID = NULL, ImportValue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "EmployeeThirdPartyFormatCheckLocation", objectId = EmployeeThirdPartyFormatCheckLocationID, body = list(DataObject = body), searchFields = append("EmployeeThirdPartyFormatCheckLocationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EmployeeThirdPartyFormatGenders
	#'
	#' This function returns a dataframe or json object of EmployeeThirdPartyFormatGenders
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeThirdPartyFormatGenders. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeThirdPartyFormatGenders.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeThirdPartyFormatGender') to get more field paths.
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
	#' @concept Employee
	#' @return A list of EmployeeThirdPartyFormatGenders
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEmployeeThirdPartyFormatGenders <- function(searchConditionsList = NULL, EmployeeThirdPartyFormatGenderID = F, EmployeeThirdPartyFormatID = F, ImportValue = F, Gender = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "EmployeeThirdPartyFormatGender", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EmployeeThirdPartyFormatGender
	#'
	#' This function returns a dataframe or json object of an EmployeeThirdPartyFormatGender
	#' @param EmployeeThirdPartyFormatGenderID The ID of the EmployeeThirdPartyFormatGender to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeThirdPartyFormatGender. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeThirdPartyFormatGender.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeThirdPartyFormatGender') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of EmployeeThirdPartyFormatGender
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEmployeeThirdPartyFormatGender <- function(EmployeeThirdPartyFormatGenderID, EmployeeThirdPartyFormatID = F, ImportValue = F, Gender = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EmployeeThirdPartyFormatGenderID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "EmployeeThirdPartyFormatGender", objectId = EmployeeThirdPartyFormatGenderID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EmployeeThirdPartyFormatGender
	#'
	#' This function deletes an EmployeeThirdPartyFormatGender
	#' @param EmployeeThirdPartyFormatGenderID The ID of the EmployeeThirdPartyFormatGender to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The EmployeeThirdPartyFormatGenderID of the deleted EmployeeThirdPartyFormatGender.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEmployeeThirdPartyFormatGender <- function(EmployeeThirdPartyFormatGenderID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "EmployeeThirdPartyFormatGender", objectId = EmployeeThirdPartyFormatGenderID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EmployeeThirdPartyFormatGender
	#'
	#' This function creates an EmployeeThirdPartyFormatGender
	#' @param fieldNames The field values to give the created EmployeeThirdPartyFormatGender. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created EmployeeThirdPartyFormatGender
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEmployeeThirdPartyFormatGender <- function(EmployeeThirdPartyFormatID = NULL, ImportValue = NULL, Gender = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "EmployeeThirdPartyFormatGender", body = list(DataObject = body), searchFields = append("EmployeeThirdPartyFormatGenderID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EmployeeThirdPartyFormatGender
	#'
	#' This function modifies an EmployeeThirdPartyFormatGender
	#' @param fieldNames The field values to give the modified EmployeeThirdPartyFormatGender. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified EmployeeThirdPartyFormatGender
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEmployeeThirdPartyFormatGender <- function(EmployeeThirdPartyFormatGenderID, EmployeeThirdPartyFormatID = NULL, ImportValue = NULL, Gender = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "EmployeeThirdPartyFormatGender", objectId = EmployeeThirdPartyFormatGenderID, body = list(DataObject = body), searchFields = append("EmployeeThirdPartyFormatGenderID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ServiceRecords
	#'
	#' This function returns a dataframe or json object of ServiceRecords
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ServiceRecords. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ServiceRecords.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ServiceRecord') to get more field paths.
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
	#' @concept Employee
	#' @return A list of ServiceRecords
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listServiceRecords <- function(searchConditionsList = NULL, ServiceRecordID = F, DistrictID = F, EmployeeID = F, SchoolYearValue = F, SchoolDistrictName = F, PositionHeld = F, PositionHeldLine2 = F, ServiceDateFrom = F, ServiceDateTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FiscalYearID = F, SchoolYearDescription = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "ServiceRecord", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ServiceRecord
	#'
	#' This function returns a dataframe or json object of a ServiceRecord
	#' @param ServiceRecordID The ID of the ServiceRecord to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ServiceRecord. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ServiceRecord.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ServiceRecord') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of ServiceRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getServiceRecord <- function(ServiceRecordID, DistrictID = F, EmployeeID = F, SchoolYearValue = F, SchoolDistrictName = F, PositionHeld = F, PositionHeldLine2 = F, ServiceDateFrom = F, ServiceDateTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FiscalYearID = F, SchoolYearDescription = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ServiceRecordID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "ServiceRecord", objectId = ServiceRecordID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ServiceRecord
	#'
	#' This function deletes a ServiceRecord
	#' @param ServiceRecordID The ID of the ServiceRecord to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The ServiceRecordID of the deleted ServiceRecord.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteServiceRecord <- function(ServiceRecordID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "ServiceRecord", objectId = ServiceRecordID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ServiceRecord
	#'
	#' This function creates a ServiceRecord
	#' @param fieldNames The field values to give the created ServiceRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created ServiceRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createServiceRecord <- function(DistrictID = NULL, EmployeeID = NULL, SchoolYearValue = NULL, SchoolDistrictName = NULL, PositionHeld = NULL, PositionHeldLine2 = NULL, ServiceDateFrom = NULL, ServiceDateTo = NULL, FiscalYearID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "ServiceRecord", body = list(DataObject = body), searchFields = append("ServiceRecordID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ServiceRecord
	#'
	#' This function modifies a ServiceRecord
	#' @param fieldNames The field values to give the modified ServiceRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified ServiceRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyServiceRecord <- function(ServiceRecordID, DistrictID = NULL, EmployeeID = NULL, SchoolYearValue = NULL, SchoolDistrictName = NULL, PositionHeld = NULL, PositionHeldLine2 = NULL, ServiceDateFrom = NULL, ServiceDateTo = NULL, FiscalYearID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "ServiceRecord", objectId = ServiceRecordID, body = list(DataObject = body), searchFields = append("ServiceRecordID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EmployeeThirdPartyImports
	#'
	#' This function returns a dataframe or json object of EmployeeThirdPartyImports
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeThirdPartyImports. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeThirdPartyImports.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeThirdPartyImport') to get more field paths.
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
	#' @concept Employee
	#' @return A list of EmployeeThirdPartyImports
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEmployeeThirdPartyImports <- function(searchConditionsList = NULL, EmployeeThirdPartyImportID = F, EmployeeThirdPartyFormatID = F, ImportTime = F, ImportData = F, MediaID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "EmployeeThirdPartyImport", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EmployeeThirdPartyImport
	#'
	#' This function returns a dataframe or json object of an EmployeeThirdPartyImport
	#' @param EmployeeThirdPartyImportID The ID of the EmployeeThirdPartyImport to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeThirdPartyImport. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeThirdPartyImport.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeThirdPartyImport') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of EmployeeThirdPartyImport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEmployeeThirdPartyImport <- function(EmployeeThirdPartyImportID, EmployeeThirdPartyFormatID = F, ImportTime = F, ImportData = F, MediaID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EmployeeThirdPartyImportID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "EmployeeThirdPartyImport", objectId = EmployeeThirdPartyImportID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EmployeeThirdPartyImport
	#'
	#' This function deletes an EmployeeThirdPartyImport
	#' @param EmployeeThirdPartyImportID The ID of the EmployeeThirdPartyImport to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The EmployeeThirdPartyImportID of the deleted EmployeeThirdPartyImport.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEmployeeThirdPartyImport <- function(EmployeeThirdPartyImportID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "EmployeeThirdPartyImport", objectId = EmployeeThirdPartyImportID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EmployeeThirdPartyImport
	#'
	#' This function creates an EmployeeThirdPartyImport
	#' @param fieldNames The field values to give the created EmployeeThirdPartyImport. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created EmployeeThirdPartyImport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEmployeeThirdPartyImport <- function(EmployeeThirdPartyFormatID = NULL, ImportTime = NULL, MediaID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "EmployeeThirdPartyImport", body = list(DataObject = body), searchFields = append("EmployeeThirdPartyImportID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EmployeeThirdPartyImport
	#'
	#' This function modifies an EmployeeThirdPartyImport
	#' @param fieldNames The field values to give the modified EmployeeThirdPartyImport. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified EmployeeThirdPartyImport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEmployeeThirdPartyImport <- function(EmployeeThirdPartyImportID, EmployeeThirdPartyFormatID = NULL, ImportTime = NULL, MediaID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "EmployeeThirdPartyImport", objectId = EmployeeThirdPartyImportID, body = list(DataObject = body), searchFields = append("EmployeeThirdPartyImportID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EmployeeFixedLengthFileFormats
	#'
	#' This function returns a dataframe or json object of EmployeeFixedLengthFileFormats
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeFixedLengthFileFormats. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeFixedLengthFileFormats.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeFixedLengthFileFormat') to get more field paths.
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
	#' @concept Employee
	#' @return A list of EmployeeFixedLengthFileFormats
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEmployeeFixedLengthFileFormats <- function(searchConditionsList = NULL, EmployeeFixedLengthFileFormatID = F, SkywardID = F, SkywardHash = F, EmployeeThirdPartyFormatID = F, NumberOfHeaderRows = F, NamePrefixStartPosition = F, NamePrefixLength = F, LastNameStartPosition = F, LastNameLength = F, FirstNameStartPosition = F, FirstNameLength = F, MiddleNameStartPosition = F, MiddleNameLength = F, NameSuffixStartPosition = F, NameSuffixLength = F, SocialSecurityNumberStartPosition = F, SocialSecurityNumberLength = F, FederalEINStartPosition = F, FederalEINLength = F, BirthDateStartPosition = F, BirthDateLength = F, GenderStartPosition = F, GenderLength = F, PhoneNumberStartPosition = F, PhoneNumberLength = F, PhoneTypeStartPosition = F, PhoneTypeLength = F, ExtensionStartPosition = F, ExtensionLength = F, EmailAddressStartPosition = F, EmailAddressLength = F, EmailTypeStartPosition = F, EmailTypeLength = F, AddressTypeCodeStartPosition = F, AddressTypeCodeLength = F, StreetNumberStartPosition = F, StreetNumberLength = F, DirectionalCodeStartPosition = F, DirectionalCodeLength = F, StreetNameStartPosition = F, StreetNameLength = F, AddressSecondaryUnitStartPosition = F, AddressSecondaryUnitLength = F, SecondaryUnitNumberStartPosition = F, SecondaryUnitNumberLength = F, AddressLineTwoStartPosition = F, AddressLineTwoLength = F, CityStartPosition = F, CityLength = F, StateStartPosition = F, StateLength = F, ZipCodeStartPosition = F, ZipCodeLength = F, CheckLocationStartPosition = F, CheckLocationLength = F, EmployeeNumberStartPosition = F, EmployeeNumberLength = F, StartDateStartPosition = F, StartDateLength = F, HireDateStartPosition = F, HireDateLength = F, I9DateStartPosition = F, I9DateLength = F, W4DateStartPosition = F, W4DateLength = F, VendorNumberStartPosition = F, VendorNumberLength = F, UsernameStartPosition = F, UsernameLength = F, AllowEmployeeAccessStartPosition = F, AllowEmployeeAccessLength = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsAlaskanStartPosition = F, IsAlaskanLength = F, IsAsianStartPosition = F, IsAsianLength = F, IsBlackStartPosition = F, IsBlackLength = F, IsHawaiianStartPosition = F, IsHawaiianLength = F, IsHispanicStartPosition = F, IsHispanicLength = F, IsWhiteStartPosition = F, IsWhiteLength = F, ZipCodeAddOnStartPosition = F, ZipCodeAddOnLength = F, PhoneNumber2StartPosition = F, PhoneNumber2Length = F, PhoneType2StartPosition = F, PhoneType2Length = F, Extension2StartPosition = F, Extension2Length = F, EmailAddress2StartPosition = F, EmailAddress2Length = F, EmailType2StartPosition = F, EmailType2Length = F, PhoneNumber3StartPosition = F, PhoneNumber3Length = F, PhoneType3StartPosition = F, PhoneType3Length = F, Extension3StartPosition = F, Extension3Length = F, EmailAddress3StartPosition = F, EmailAddress3Length = F, EmailType3StartPosition = F, EmailType3Length = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "EmployeeFixedLengthFileFormat", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EmployeeFixedLengthFileFormat
	#'
	#' This function returns a dataframe or json object of an EmployeeFixedLengthFileFormat
	#' @param EmployeeFixedLengthFileFormatID The ID of the EmployeeFixedLengthFileFormat to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeFixedLengthFileFormat. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeFixedLengthFileFormat.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeFixedLengthFileFormat') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of EmployeeFixedLengthFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEmployeeFixedLengthFileFormat <- function(EmployeeFixedLengthFileFormatID, SkywardID = F, SkywardHash = F, EmployeeThirdPartyFormatID = F, NumberOfHeaderRows = F, NamePrefixStartPosition = F, NamePrefixLength = F, LastNameStartPosition = F, LastNameLength = F, FirstNameStartPosition = F, FirstNameLength = F, MiddleNameStartPosition = F, MiddleNameLength = F, NameSuffixStartPosition = F, NameSuffixLength = F, SocialSecurityNumberStartPosition = F, SocialSecurityNumberLength = F, FederalEINStartPosition = F, FederalEINLength = F, BirthDateStartPosition = F, BirthDateLength = F, GenderStartPosition = F, GenderLength = F, PhoneNumberStartPosition = F, PhoneNumberLength = F, PhoneTypeStartPosition = F, PhoneTypeLength = F, ExtensionStartPosition = F, ExtensionLength = F, EmailAddressStartPosition = F, EmailAddressLength = F, EmailTypeStartPosition = F, EmailTypeLength = F, AddressTypeCodeStartPosition = F, AddressTypeCodeLength = F, StreetNumberStartPosition = F, StreetNumberLength = F, DirectionalCodeStartPosition = F, DirectionalCodeLength = F, StreetNameStartPosition = F, StreetNameLength = F, AddressSecondaryUnitStartPosition = F, AddressSecondaryUnitLength = F, SecondaryUnitNumberStartPosition = F, SecondaryUnitNumberLength = F, AddressLineTwoStartPosition = F, AddressLineTwoLength = F, CityStartPosition = F, CityLength = F, StateStartPosition = F, StateLength = F, ZipCodeStartPosition = F, ZipCodeLength = F, CheckLocationStartPosition = F, CheckLocationLength = F, EmployeeNumberStartPosition = F, EmployeeNumberLength = F, StartDateStartPosition = F, StartDateLength = F, HireDateStartPosition = F, HireDateLength = F, I9DateStartPosition = F, I9DateLength = F, W4DateStartPosition = F, W4DateLength = F, VendorNumberStartPosition = F, VendorNumberLength = F, UsernameStartPosition = F, UsernameLength = F, AllowEmployeeAccessStartPosition = F, AllowEmployeeAccessLength = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsAlaskanStartPosition = F, IsAlaskanLength = F, IsAsianStartPosition = F, IsAsianLength = F, IsBlackStartPosition = F, IsBlackLength = F, IsHawaiianStartPosition = F, IsHawaiianLength = F, IsHispanicStartPosition = F, IsHispanicLength = F, IsWhiteStartPosition = F, IsWhiteLength = F, ZipCodeAddOnStartPosition = F, ZipCodeAddOnLength = F, PhoneNumber2StartPosition = F, PhoneNumber2Length = F, PhoneType2StartPosition = F, PhoneType2Length = F, Extension2StartPosition = F, Extension2Length = F, EmailAddress2StartPosition = F, EmailAddress2Length = F, EmailType2StartPosition = F, EmailType2Length = F, PhoneNumber3StartPosition = F, PhoneNumber3Length = F, PhoneType3StartPosition = F, PhoneType3Length = F, Extension3StartPosition = F, Extension3Length = F, EmailAddress3StartPosition = F, EmailAddress3Length = F, EmailType3StartPosition = F, EmailType3Length = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EmployeeFixedLengthFileFormatID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "EmployeeFixedLengthFileFormat", objectId = EmployeeFixedLengthFileFormatID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EmployeeFixedLengthFileFormat
	#'
	#' This function deletes an EmployeeFixedLengthFileFormat
	#' @param EmployeeFixedLengthFileFormatID The ID of the EmployeeFixedLengthFileFormat to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The EmployeeFixedLengthFileFormatID of the deleted EmployeeFixedLengthFileFormat.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEmployeeFixedLengthFileFormat <- function(EmployeeFixedLengthFileFormatID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "EmployeeFixedLengthFileFormat", objectId = EmployeeFixedLengthFileFormatID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EmployeeFixedLengthFileFormat
	#'
	#' This function creates an EmployeeFixedLengthFileFormat
	#' @param fieldNames The field values to give the created EmployeeFixedLengthFileFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created EmployeeFixedLengthFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEmployeeFixedLengthFileFormat <- function(EmployeeThirdPartyFormatID = NULL, NumberOfHeaderRows = NULL, NamePrefixStartPosition = NULL, NamePrefixLength = NULL, LastNameStartPosition = NULL, LastNameLength = NULL, FirstNameStartPosition = NULL, FirstNameLength = NULL, MiddleNameStartPosition = NULL, MiddleNameLength = NULL, NameSuffixStartPosition = NULL, NameSuffixLength = NULL, SocialSecurityNumberStartPosition = NULL, SocialSecurityNumberLength = NULL, FederalEINStartPosition = NULL, FederalEINLength = NULL, BirthDateStartPosition = NULL, BirthDateLength = NULL, GenderStartPosition = NULL, GenderLength = NULL, PhoneNumberStartPosition = NULL, PhoneNumberLength = NULL, PhoneTypeStartPosition = NULL, PhoneTypeLength = NULL, ExtensionStartPosition = NULL, ExtensionLength = NULL, EmailAddressStartPosition = NULL, EmailAddressLength = NULL, EmailTypeStartPosition = NULL, EmailTypeLength = NULL, AddressTypeCodeStartPosition = NULL, AddressTypeCodeLength = NULL, StreetNumberStartPosition = NULL, StreetNumberLength = NULL, DirectionalCodeStartPosition = NULL, DirectionalCodeLength = NULL, StreetNameStartPosition = NULL, StreetNameLength = NULL, AddressSecondaryUnitStartPosition = NULL, AddressSecondaryUnitLength = NULL, SecondaryUnitNumberStartPosition = NULL, SecondaryUnitNumberLength = NULL, AddressLineTwoStartPosition = NULL, AddressLineTwoLength = NULL, CityStartPosition = NULL, CityLength = NULL, StateStartPosition = NULL, StateLength = NULL, ZipCodeStartPosition = NULL, ZipCodeLength = NULL, CheckLocationStartPosition = NULL, CheckLocationLength = NULL, EmployeeNumberStartPosition = NULL, EmployeeNumberLength = NULL, StartDateStartPosition = NULL, StartDateLength = NULL, HireDateStartPosition = NULL, HireDateLength = NULL, I9DateStartPosition = NULL, I9DateLength = NULL, W4DateStartPosition = NULL, W4DateLength = NULL, VendorNumberStartPosition = NULL, VendorNumberLength = NULL, UsernameStartPosition = NULL, UsernameLength = NULL, AllowEmployeeAccessStartPosition = NULL, AllowEmployeeAccessLength = NULL, IsAlaskanStartPosition = NULL, IsAlaskanLength = NULL, IsAsianStartPosition = NULL, IsAsianLength = NULL, IsBlackStartPosition = NULL, IsBlackLength = NULL, IsHawaiianStartPosition = NULL, IsHawaiianLength = NULL, IsHispanicStartPosition = NULL, IsHispanicLength = NULL, IsWhiteStartPosition = NULL, IsWhiteLength = NULL, ZipCodeAddOnStartPosition = NULL, ZipCodeAddOnLength = NULL, PhoneNumber2StartPosition = NULL, PhoneNumber2Length = NULL, PhoneType2StartPosition = NULL, PhoneType2Length = NULL, Extension2StartPosition = NULL, Extension2Length = NULL, EmailAddress2StartPosition = NULL, EmailAddress2Length = NULL, EmailType2StartPosition = NULL, EmailType2Length = NULL, PhoneNumber3StartPosition = NULL, PhoneNumber3Length = NULL, PhoneType3StartPosition = NULL, PhoneType3Length = NULL, Extension3StartPosition = NULL, Extension3Length = NULL, EmailAddress3StartPosition = NULL, EmailAddress3Length = NULL, EmailType3StartPosition = NULL, EmailType3Length = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "EmployeeFixedLengthFileFormat", body = list(DataObject = body), searchFields = append("EmployeeFixedLengthFileFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EmployeeFixedLengthFileFormat
	#'
	#' This function modifies an EmployeeFixedLengthFileFormat
	#' @param fieldNames The field values to give the modified EmployeeFixedLengthFileFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified EmployeeFixedLengthFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEmployeeFixedLengthFileFormat <- function(EmployeeFixedLengthFileFormatID, EmployeeThirdPartyFormatID = NULL, NumberOfHeaderRows = NULL, NamePrefixStartPosition = NULL, NamePrefixLength = NULL, LastNameStartPosition = NULL, LastNameLength = NULL, FirstNameStartPosition = NULL, FirstNameLength = NULL, MiddleNameStartPosition = NULL, MiddleNameLength = NULL, NameSuffixStartPosition = NULL, NameSuffixLength = NULL, SocialSecurityNumberStartPosition = NULL, SocialSecurityNumberLength = NULL, FederalEINStartPosition = NULL, FederalEINLength = NULL, BirthDateStartPosition = NULL, BirthDateLength = NULL, GenderStartPosition = NULL, GenderLength = NULL, PhoneNumberStartPosition = NULL, PhoneNumberLength = NULL, PhoneTypeStartPosition = NULL, PhoneTypeLength = NULL, ExtensionStartPosition = NULL, ExtensionLength = NULL, EmailAddressStartPosition = NULL, EmailAddressLength = NULL, EmailTypeStartPosition = NULL, EmailTypeLength = NULL, AddressTypeCodeStartPosition = NULL, AddressTypeCodeLength = NULL, StreetNumberStartPosition = NULL, StreetNumberLength = NULL, DirectionalCodeStartPosition = NULL, DirectionalCodeLength = NULL, StreetNameStartPosition = NULL, StreetNameLength = NULL, AddressSecondaryUnitStartPosition = NULL, AddressSecondaryUnitLength = NULL, SecondaryUnitNumberStartPosition = NULL, SecondaryUnitNumberLength = NULL, AddressLineTwoStartPosition = NULL, AddressLineTwoLength = NULL, CityStartPosition = NULL, CityLength = NULL, StateStartPosition = NULL, StateLength = NULL, ZipCodeStartPosition = NULL, ZipCodeLength = NULL, CheckLocationStartPosition = NULL, CheckLocationLength = NULL, EmployeeNumberStartPosition = NULL, EmployeeNumberLength = NULL, StartDateStartPosition = NULL, StartDateLength = NULL, HireDateStartPosition = NULL, HireDateLength = NULL, I9DateStartPosition = NULL, I9DateLength = NULL, W4DateStartPosition = NULL, W4DateLength = NULL, VendorNumberStartPosition = NULL, VendorNumberLength = NULL, UsernameStartPosition = NULL, UsernameLength = NULL, AllowEmployeeAccessStartPosition = NULL, AllowEmployeeAccessLength = NULL, IsAlaskanStartPosition = NULL, IsAlaskanLength = NULL, IsAsianStartPosition = NULL, IsAsianLength = NULL, IsBlackStartPosition = NULL, IsBlackLength = NULL, IsHawaiianStartPosition = NULL, IsHawaiianLength = NULL, IsHispanicStartPosition = NULL, IsHispanicLength = NULL, IsWhiteStartPosition = NULL, IsWhiteLength = NULL, ZipCodeAddOnStartPosition = NULL, ZipCodeAddOnLength = NULL, PhoneNumber2StartPosition = NULL, PhoneNumber2Length = NULL, PhoneType2StartPosition = NULL, PhoneType2Length = NULL, Extension2StartPosition = NULL, Extension2Length = NULL, EmailAddress2StartPosition = NULL, EmailAddress2Length = NULL, EmailType2StartPosition = NULL, EmailType2Length = NULL, PhoneNumber3StartPosition = NULL, PhoneNumber3Length = NULL, PhoneType3StartPosition = NULL, PhoneType3Length = NULL, Extension3StartPosition = NULL, Extension3Length = NULL, EmailAddress3StartPosition = NULL, EmailAddress3Length = NULL, EmailType3StartPosition = NULL, EmailType3Length = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "EmployeeFixedLengthFileFormat", objectId = EmployeeFixedLengthFileFormatID, body = list(DataObject = body), searchFields = append("EmployeeFixedLengthFileFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempDegreeProgramOfStudies
	#'
	#' This function returns a dataframe or json object of TempDegreeProgramOfStudies
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempDegreeProgramOfStudies. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempDegreeProgramOfStudies.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempDegreeProgramOfStudy') to get more field paths.
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
	#' @concept Employee
	#' @return A list of TempDegreeProgramOfStudies
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempDegreeProgramOfStudies <- function(searchConditionsList = NULL, TempDegreeProgramOfStudyID = F, ProgramOfStudyID = F, TempDegreeID = F, DegreeID = F, FocusCode = F, Comment = F, LineNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "TempDegreeProgramOfStudy", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempDegreeProgramOfStudy
	#'
	#' This function returns a dataframe or json object of a TempDegreeProgramOfStudy
	#' @param TempDegreeProgramOfStudyID The ID of the TempDegreeProgramOfStudy to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempDegreeProgramOfStudy. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempDegreeProgramOfStudy.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempDegreeProgramOfStudy') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of TempDegreeProgramOfStudy
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempDegreeProgramOfStudy <- function(TempDegreeProgramOfStudyID, ProgramOfStudyID = F, TempDegreeID = F, DegreeID = F, FocusCode = F, Comment = F, LineNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempDegreeProgramOfStudyID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "TempDegreeProgramOfStudy", objectId = TempDegreeProgramOfStudyID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempDegreeProgramOfStudy
	#'
	#' This function deletes a TempDegreeProgramOfStudy
	#' @param TempDegreeProgramOfStudyID The ID of the TempDegreeProgramOfStudy to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The TempDegreeProgramOfStudyID of the deleted TempDegreeProgramOfStudy.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempDegreeProgramOfStudy <- function(TempDegreeProgramOfStudyID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "TempDegreeProgramOfStudy", objectId = TempDegreeProgramOfStudyID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempDegreeProgramOfStudy
	#'
	#' This function creates a TempDegreeProgramOfStudy
	#' @param fieldNames The field values to give the created TempDegreeProgramOfStudy. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created TempDegreeProgramOfStudy
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempDegreeProgramOfStudy <- function(ProgramOfStudyID = NULL, TempDegreeID = NULL, DegreeID = NULL, FocusCode = NULL, Comment = NULL, LineNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "TempDegreeProgramOfStudy", body = list(DataObject = body), searchFields = append("TempDegreeProgramOfStudyID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempDegreeProgramOfStudy
	#'
	#' This function modifies a TempDegreeProgramOfStudy
	#' @param fieldNames The field values to give the modified TempDegreeProgramOfStudy. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified TempDegreeProgramOfStudy
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempDegreeProgramOfStudy <- function(TempDegreeProgramOfStudyID, ProgramOfStudyID = NULL, TempDegreeID = NULL, DegreeID = NULL, FocusCode = NULL, Comment = NULL, LineNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "TempDegreeProgramOfStudy", objectId = TempDegreeProgramOfStudyID, body = list(DataObject = body), searchFields = append("TempDegreeProgramOfStudyID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempServiceRecords
	#'
	#' This function returns a dataframe or json object of TempServiceRecords
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempServiceRecords. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempServiceRecords.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempServiceRecord') to get more field paths.
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
	#' @concept Employee
	#' @return A list of TempServiceRecords
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempServiceRecords <- function(searchConditionsList = NULL, TempServiceRecordID = F, ServiceRecordID = F, DistrictID = F, FiscalYearID = F, EmployeeID = F, EmployeeFullNameFML = F, SchoolYearValue = F, SchoolDistrictName = F, PositionHeld = F, PositionHeldLine2 = F, ServiceDateFrom = F, ServiceDateTo = F, SchoolDistrictTypeCode = F, StateID = F, StateDisplayName = F, CountyID = F, CountyName = F, YearsOfExperience = F, PercentOfDayEmployed = F, NumberOfDaysEmployed = F, StateSickLeavePriorYearBalance = F, StateSickLeaveEarned = F, StateSickLeaveUsed = F, StatePersonalLeavePriorYearBalance = F, StatePersonalLeaveEarned = F, StatePersonalLeaveUsed = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeNumber = F, HasExistingServiceRecord = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "TempServiceRecord", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempServiceRecord
	#'
	#' This function returns a dataframe or json object of a TempServiceRecord
	#' @param TempServiceRecordID The ID of the TempServiceRecord to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempServiceRecord. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempServiceRecord.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempServiceRecord') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of TempServiceRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempServiceRecord <- function(TempServiceRecordID, ServiceRecordID = F, DistrictID = F, FiscalYearID = F, EmployeeID = F, EmployeeFullNameFML = F, SchoolYearValue = F, SchoolDistrictName = F, PositionHeld = F, PositionHeldLine2 = F, ServiceDateFrom = F, ServiceDateTo = F, SchoolDistrictTypeCode = F, StateID = F, StateDisplayName = F, CountyID = F, CountyName = F, YearsOfExperience = F, PercentOfDayEmployed = F, NumberOfDaysEmployed = F, StateSickLeavePriorYearBalance = F, StateSickLeaveEarned = F, StateSickLeaveUsed = F, StatePersonalLeavePriorYearBalance = F, StatePersonalLeaveEarned = F, StatePersonalLeaveUsed = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeNumber = F, HasExistingServiceRecord = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempServiceRecordID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "TempServiceRecord", objectId = TempServiceRecordID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempServiceRecord
	#'
	#' This function deletes a TempServiceRecord
	#' @param TempServiceRecordID The ID of the TempServiceRecord to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The TempServiceRecordID of the deleted TempServiceRecord.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempServiceRecord <- function(TempServiceRecordID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "TempServiceRecord", objectId = TempServiceRecordID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempServiceRecord
	#'
	#' This function creates a TempServiceRecord
	#' @param fieldNames The field values to give the created TempServiceRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created TempServiceRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempServiceRecord <- function(ServiceRecordID = NULL, DistrictID = NULL, FiscalYearID = NULL, EmployeeID = NULL, EmployeeFullNameFML = NULL, SchoolYearValue = NULL, SchoolDistrictName = NULL, PositionHeld = NULL, PositionHeldLine2 = NULL, ServiceDateFrom = NULL, ServiceDateTo = NULL, SchoolDistrictTypeCode = NULL, StateID = NULL, StateDisplayName = NULL, CountyID = NULL, CountyName = NULL, YearsOfExperience = NULL, PercentOfDayEmployed = NULL, NumberOfDaysEmployed = NULL, StateSickLeavePriorYearBalance = NULL, StateSickLeaveEarned = NULL, StateSickLeaveUsed = NULL, StatePersonalLeavePriorYearBalance = NULL, StatePersonalLeaveEarned = NULL, StatePersonalLeaveUsed = NULL, EmployeeNumber = NULL, HasExistingServiceRecord = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "TempServiceRecord", body = list(DataObject = body), searchFields = append("TempServiceRecordID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempServiceRecord
	#'
	#' This function modifies a TempServiceRecord
	#' @param fieldNames The field values to give the modified TempServiceRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified TempServiceRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempServiceRecord <- function(TempServiceRecordID, ServiceRecordID = NULL, DistrictID = NULL, FiscalYearID = NULL, EmployeeID = NULL, EmployeeFullNameFML = NULL, SchoolYearValue = NULL, SchoolDistrictName = NULL, PositionHeld = NULL, PositionHeldLine2 = NULL, ServiceDateFrom = NULL, ServiceDateTo = NULL, SchoolDistrictTypeCode = NULL, StateID = NULL, StateDisplayName = NULL, CountyID = NULL, CountyName = NULL, YearsOfExperience = NULL, PercentOfDayEmployed = NULL, NumberOfDaysEmployed = NULL, StateSickLeavePriorYearBalance = NULL, StateSickLeaveEarned = NULL, StateSickLeaveUsed = NULL, StatePersonalLeavePriorYearBalance = NULL, StatePersonalLeaveEarned = NULL, StatePersonalLeaveUsed = NULL, EmployeeNumber = NULL, HasExistingServiceRecord = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "TempServiceRecord", objectId = TempServiceRecordID, body = list(DataObject = body), searchFields = append("TempServiceRecordID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempEmployees
	#'
	#' This function returns a dataframe or json object of TempEmployees
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempEmployees. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempEmployees.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempEmployee') to get more field paths.
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
	#' @concept Employee
	#' @return A list of TempEmployees
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempEmployees <- function(searchConditionsList = NULL, TempEmployeeID = F, EmployeeID = F, IsActive = F, EmployeeNumber = F, FullAddress = F, HasErrors = F, IsValidToDelete = F, ErrorCount = F, Deleted = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TempNameID = F, AllowEmployeeAccess = F, LastName = F, FirstName = F, CurrentAssignmentPrimaryPositionTypeCode = F, CurrentAssignmentPrimaryPositionDistributionsAssignmentTypeCodes = F, CurrentAssignmentPrimaryPositionDistributionsBuildingCodes = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "TempEmployee", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempEmployee
	#'
	#' This function returns a dataframe or json object of a TempEmployee
	#' @param TempEmployeeID The ID of the TempEmployee to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempEmployee. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempEmployee.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempEmployee') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of TempEmployee
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempEmployee <- function(TempEmployeeID, EmployeeID = F, IsActive = F, EmployeeNumber = F, FullAddress = F, HasErrors = F, IsValidToDelete = F, ErrorCount = F, Deleted = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TempNameID = F, AllowEmployeeAccess = F, LastName = F, FirstName = F, CurrentAssignmentPrimaryPositionTypeCode = F, CurrentAssignmentPrimaryPositionDistributionsAssignmentTypeCodes = F, CurrentAssignmentPrimaryPositionDistributionsBuildingCodes = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempEmployeeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "TempEmployee", objectId = TempEmployeeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempEmployee
	#'
	#' This function deletes a TempEmployee
	#' @param TempEmployeeID The ID of the TempEmployee to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The TempEmployeeID of the deleted TempEmployee.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempEmployee <- function(TempEmployeeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "TempEmployee", objectId = TempEmployeeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempEmployee
	#'
	#' This function creates a TempEmployee
	#' @param fieldNames The field values to give the created TempEmployee. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created TempEmployee
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempEmployee <- function(EmployeeID = NULL, IsActive = NULL, EmployeeNumber = NULL, FullAddress = NULL, HasErrors = NULL, IsValidToDelete = NULL, ErrorCount = NULL, TempNameID = NULL, AllowEmployeeAccess = NULL, LastName = NULL, FirstName = NULL, CurrentAssignmentPrimaryPositionTypeCode = NULL, CurrentAssignmentPrimaryPositionDistributionsAssignmentTypeCodes = NULL, CurrentAssignmentPrimaryPositionDistributionsBuildingCodes = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "TempEmployee", body = list(DataObject = body), searchFields = append("TempEmployeeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempEmployee
	#'
	#' This function modifies a TempEmployee
	#' @param fieldNames The field values to give the modified TempEmployee. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified TempEmployee
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempEmployee <- function(TempEmployeeID, EmployeeID = NULL, IsActive = NULL, EmployeeNumber = NULL, FullAddress = NULL, HasErrors = NULL, IsValidToDelete = NULL, ErrorCount = NULL, TempNameID = NULL, AllowEmployeeAccess = NULL, LastName = NULL, FirstName = NULL, CurrentAssignmentPrimaryPositionTypeCode = NULL, CurrentAssignmentPrimaryPositionDistributionsAssignmentTypeCodes = NULL, CurrentAssignmentPrimaryPositionDistributionsBuildingCodes = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "TempEmployee", objectId = TempEmployeeID, body = list(DataObject = body), searchFields = append("TempEmployeeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempEmployeeErrors
	#'
	#' This function returns a dataframe or json object of TempEmployeeErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempEmployeeErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempEmployeeErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempEmployeeError') to get more field paths.
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
	#' @concept Employee
	#' @return A list of TempEmployeeErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempEmployeeErrors <- function(searchConditionsList = NULL, TempEmployeeErrorID = F, TempEmployeeID = F, EmployeeID = F, FullNameLFM = F, Error = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "TempEmployeeError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempEmployeeError
	#'
	#' This function returns a dataframe or json object of a TempEmployeeError
	#' @param TempEmployeeErrorID The ID of the TempEmployeeError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempEmployeeError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempEmployeeError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempEmployeeError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of TempEmployeeError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempEmployeeError <- function(TempEmployeeErrorID, TempEmployeeID = F, EmployeeID = F, FullNameLFM = F, Error = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempEmployeeErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "TempEmployeeError", objectId = TempEmployeeErrorID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempEmployeeError
	#'
	#' This function deletes a TempEmployeeError
	#' @param TempEmployeeErrorID The ID of the TempEmployeeError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The TempEmployeeErrorID of the deleted TempEmployeeError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempEmployeeError <- function(TempEmployeeErrorID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "TempEmployeeError", objectId = TempEmployeeErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempEmployeeError
	#'
	#' This function creates a TempEmployeeError
	#' @param fieldNames The field values to give the created TempEmployeeError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created TempEmployeeError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempEmployeeError <- function(TempEmployeeID = NULL, EmployeeID = NULL, FullNameLFM = NULL, Error = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "TempEmployeeError", body = list(DataObject = body), searchFields = append("TempEmployeeErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempEmployeeError
	#'
	#' This function modifies a TempEmployeeError
	#' @param fieldNames The field values to give the modified TempEmployeeError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified TempEmployeeError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempEmployeeError <- function(TempEmployeeErrorID, TempEmployeeID = NULL, EmployeeID = NULL, FullNameLFM = NULL, Error = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "TempEmployeeError", objectId = TempEmployeeErrorID, body = list(DataObject = body), searchFields = append("TempEmployeeErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempEmployeeYearExperiences
	#'
	#' This function returns a dataframe or json object of TempEmployeeYearExperiences
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempEmployeeYearExperiences. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempEmployeeYearExperiences.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempEmployeeYearExperience') to get more field paths.
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
	#' @concept Employee
	#' @return A list of TempEmployeeYearExperiences
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempEmployeeYearExperiences <- function(searchConditionsList = NULL, TempEmployeeYearExperienceID = F, EmployeeYearExperienceID = F, DistrictID = F, FiscalYearDescription = F, EmployeeID = F, EmployeeName = F, EmployeeNumber = F, Amount = F, PreviousAmount = F, YearExperienceLabelID = F, YearExperienceLabelName = F, ErrorCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "TempEmployeeYearExperience", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempEmployeeYearExperience
	#'
	#' This function returns a dataframe or json object of a TempEmployeeYearExperience
	#' @param TempEmployeeYearExperienceID The ID of the TempEmployeeYearExperience to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempEmployeeYearExperience. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempEmployeeYearExperience.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempEmployeeYearExperience') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of TempEmployeeYearExperience
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempEmployeeYearExperience <- function(TempEmployeeYearExperienceID, EmployeeYearExperienceID = F, DistrictID = F, FiscalYearDescription = F, EmployeeID = F, EmployeeName = F, EmployeeNumber = F, Amount = F, PreviousAmount = F, YearExperienceLabelID = F, YearExperienceLabelName = F, ErrorCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempEmployeeYearExperienceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "TempEmployeeYearExperience", objectId = TempEmployeeYearExperienceID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempEmployeeYearExperience
	#'
	#' This function deletes a TempEmployeeYearExperience
	#' @param TempEmployeeYearExperienceID The ID of the TempEmployeeYearExperience to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The TempEmployeeYearExperienceID of the deleted TempEmployeeYearExperience.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempEmployeeYearExperience <- function(TempEmployeeYearExperienceID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "TempEmployeeYearExperience", objectId = TempEmployeeYearExperienceID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempEmployeeYearExperience
	#'
	#' This function creates a TempEmployeeYearExperience
	#' @param fieldNames The field values to give the created TempEmployeeYearExperience. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created TempEmployeeYearExperience
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempEmployeeYearExperience <- function(EmployeeYearExperienceID = NULL, DistrictID = NULL, FiscalYearDescription = NULL, EmployeeID = NULL, EmployeeName = NULL, EmployeeNumber = NULL, Amount = NULL, PreviousAmount = NULL, YearExperienceLabelID = NULL, YearExperienceLabelName = NULL, ErrorCount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "TempEmployeeYearExperience", body = list(DataObject = body), searchFields = append("TempEmployeeYearExperienceID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempEmployeeYearExperience
	#'
	#' This function modifies a TempEmployeeYearExperience
	#' @param fieldNames The field values to give the modified TempEmployeeYearExperience. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified TempEmployeeYearExperience
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempEmployeeYearExperience <- function(TempEmployeeYearExperienceID, EmployeeYearExperienceID = NULL, DistrictID = NULL, FiscalYearDescription = NULL, EmployeeID = NULL, EmployeeName = NULL, EmployeeNumber = NULL, Amount = NULL, PreviousAmount = NULL, YearExperienceLabelID = NULL, YearExperienceLabelName = NULL, ErrorCount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "TempEmployeeYearExperience", objectId = TempEmployeeYearExperienceID, body = list(DataObject = body), searchFields = append("TempEmployeeYearExperienceID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempEmployeeYearExperienceErrors
	#'
	#' This function returns a dataframe or json object of TempEmployeeYearExperienceErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempEmployeeYearExperienceErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempEmployeeYearExperienceErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempEmployeeYearExperienceError') to get more field paths.
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
	#' @concept Employee
	#' @return A list of TempEmployeeYearExperienceErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempEmployeeYearExperienceErrors <- function(searchConditionsList = NULL, TempEmployeeYearExperienceErrorID = F, TempEmployeeYearExperienceID = F, Error = F, ErrorDetail = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "TempEmployeeYearExperienceError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempEmployeeYearExperienceError
	#'
	#' This function returns a dataframe or json object of a TempEmployeeYearExperienceError
	#' @param TempEmployeeYearExperienceErrorID The ID of the TempEmployeeYearExperienceError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempEmployeeYearExperienceError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempEmployeeYearExperienceError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempEmployeeYearExperienceError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of TempEmployeeYearExperienceError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempEmployeeYearExperienceError <- function(TempEmployeeYearExperienceErrorID, TempEmployeeYearExperienceID = F, Error = F, ErrorDetail = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempEmployeeYearExperienceErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "TempEmployeeYearExperienceError", objectId = TempEmployeeYearExperienceErrorID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempEmployeeYearExperienceError
	#'
	#' This function deletes a TempEmployeeYearExperienceError
	#' @param TempEmployeeYearExperienceErrorID The ID of the TempEmployeeYearExperienceError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The TempEmployeeYearExperienceErrorID of the deleted TempEmployeeYearExperienceError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempEmployeeYearExperienceError <- function(TempEmployeeYearExperienceErrorID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "TempEmployeeYearExperienceError", objectId = TempEmployeeYearExperienceErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempEmployeeYearExperienceError
	#'
	#' This function creates a TempEmployeeYearExperienceError
	#' @param fieldNames The field values to give the created TempEmployeeYearExperienceError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created TempEmployeeYearExperienceError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempEmployeeYearExperienceError <- function(TempEmployeeYearExperienceID = NULL, Error = NULL, ErrorDetail = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "TempEmployeeYearExperienceError", body = list(DataObject = body), searchFields = append("TempEmployeeYearExperienceErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempEmployeeYearExperienceError
	#'
	#' This function modifies a TempEmployeeYearExperienceError
	#' @param fieldNames The field values to give the modified TempEmployeeYearExperienceError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified TempEmployeeYearExperienceError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempEmployeeYearExperienceError <- function(TempEmployeeYearExperienceErrorID, TempEmployeeYearExperienceID = NULL, Error = NULL, ErrorDetail = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "TempEmployeeYearExperienceError", objectId = TempEmployeeYearExperienceErrorID, body = list(DataObject = body), searchFields = append("TempEmployeeYearExperienceErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempEmployeeImportValueSets
	#'
	#' This function returns a dataframe or json object of TempEmployeeImportValueSets
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempEmployeeImportValueSets. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempEmployeeImportValueSets.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempEmployeeImportValueSet') to get more field paths.
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
	#' @concept Employee
	#' @return A list of TempEmployeeImportValueSets
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempEmployeeImportValueSets <- function(searchConditionsList = NULL, TempEmployeeImportValueSetID = F, NameID = F, NameTitleID = F, NameTitleCode = F, NameSuffixID = F, NameSuffixCode = F, LastName = F, FirstName = F, MiddleName = F, SocialSecurityNumber = F, MaskedSocialSecurityNumber = F, FederalEIN = F, BirthDate = F, GenderCode = F, IsAlaskan = F, IsAsian = F, IsBlack = F, IsHawaiian = F, IsHispanic = F, IsWhite = F, UserID = F, Username = F, IsActive = F, DirectionalID = F, DirectionalCode = F, ZipID = F, ZipCode = F, City = F, StateID = F, StateCode = F, StreetID = F, StreetName = F, AddressID = F, StreetNumber = F, AddressLine2 = F, ZipCodeAddOn = F, AddressTypeCode = F, CountryCode = F, BasicAddressDisplay = F, AddressSecondaryUnitID = F, AddressSecondaryUnitCode = F, SecondaryUnitNumber = F, EmployeeID = F, EmployeeNumber = F, AllowEmployeeAccess = F, W4Date = F, I9Date = F, CheckLocationID = F, CheckLocationCode = F, StartDate = F, HireDate = F, VendorID = F, VendorNumber = F, LineNumber = F, HasErrors = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, NamePhone1ID = F, PhoneNumber1 = F, PhoneType1ID = F, PhoneTypeCode1 = F, Extension1 = F, NamePhoneRank1 = F, NamePhone2ID = F, PhoneNumber2 = F, PhoneType2ID = F, PhoneTypeCode2 = F, Extension2 = F, NamePhoneRank2 = F, NamePhone3ID = F, PhoneNumber3 = F, PhoneType3ID = F, PhoneTypeCode3 = F, Extension3 = F, NamePhoneRank3 = F, NameEmail1ID = F, EmailAddress1 = F, EmailType1ID = F, EmailTypeCode1 = F, NameEmailRank1 = F, NameEmail2ID = F, EmailAddress2 = F, EmailType2ID = F, EmailTypeCode2 = F, NameEmailRank2 = F, NameEmail3ID = F, EmailAddress3 = F, EmailType3ID = F, EmailTypeCode3 = F, NameEmailRank3 = F, CreateNameAddress = F, IsUserName = F, EmployeeThirdPartyImportID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "TempEmployeeImportValueSet", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempEmployeeImportValueSet
	#'
	#' This function returns a dataframe or json object of a TempEmployeeImportValueSet
	#' @param TempEmployeeImportValueSetID The ID of the TempEmployeeImportValueSet to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempEmployeeImportValueSet. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempEmployeeImportValueSet.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempEmployeeImportValueSet') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of TempEmployeeImportValueSet
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempEmployeeImportValueSet <- function(TempEmployeeImportValueSetID, NameID = F, NameTitleID = F, NameTitleCode = F, NameSuffixID = F, NameSuffixCode = F, LastName = F, FirstName = F, MiddleName = F, SocialSecurityNumber = F, MaskedSocialSecurityNumber = F, FederalEIN = F, BirthDate = F, GenderCode = F, IsAlaskan = F, IsAsian = F, IsBlack = F, IsHawaiian = F, IsHispanic = F, IsWhite = F, UserID = F, Username = F, IsActive = F, DirectionalID = F, DirectionalCode = F, ZipID = F, ZipCode = F, City = F, StateID = F, StateCode = F, StreetID = F, StreetName = F, AddressID = F, StreetNumber = F, AddressLine2 = F, ZipCodeAddOn = F, AddressTypeCode = F, CountryCode = F, BasicAddressDisplay = F, AddressSecondaryUnitID = F, AddressSecondaryUnitCode = F, SecondaryUnitNumber = F, EmployeeID = F, EmployeeNumber = F, AllowEmployeeAccess = F, W4Date = F, I9Date = F, CheckLocationID = F, CheckLocationCode = F, StartDate = F, HireDate = F, VendorID = F, VendorNumber = F, LineNumber = F, HasErrors = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, NamePhone1ID = F, PhoneNumber1 = F, PhoneType1ID = F, PhoneTypeCode1 = F, Extension1 = F, NamePhoneRank1 = F, NamePhone2ID = F, PhoneNumber2 = F, PhoneType2ID = F, PhoneTypeCode2 = F, Extension2 = F, NamePhoneRank2 = F, NamePhone3ID = F, PhoneNumber3 = F, PhoneType3ID = F, PhoneTypeCode3 = F, Extension3 = F, NamePhoneRank3 = F, NameEmail1ID = F, EmailAddress1 = F, EmailType1ID = F, EmailTypeCode1 = F, NameEmailRank1 = F, NameEmail2ID = F, EmailAddress2 = F, EmailType2ID = F, EmailTypeCode2 = F, NameEmailRank2 = F, NameEmail3ID = F, EmailAddress3 = F, EmailType3ID = F, EmailTypeCode3 = F, NameEmailRank3 = F, CreateNameAddress = F, IsUserName = F, EmployeeThirdPartyImportID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempEmployeeImportValueSetID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "TempEmployeeImportValueSet", objectId = TempEmployeeImportValueSetID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempEmployeeImportValueSet
	#'
	#' This function deletes a TempEmployeeImportValueSet
	#' @param TempEmployeeImportValueSetID The ID of the TempEmployeeImportValueSet to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The TempEmployeeImportValueSetID of the deleted TempEmployeeImportValueSet.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempEmployeeImportValueSet <- function(TempEmployeeImportValueSetID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "TempEmployeeImportValueSet", objectId = TempEmployeeImportValueSetID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempEmployeeImportValueSet
	#'
	#' This function creates a TempEmployeeImportValueSet
	#' @param fieldNames The field values to give the created TempEmployeeImportValueSet. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created TempEmployeeImportValueSet
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempEmployeeImportValueSet <- function(NameID = NULL, NameTitleID = NULL, NameTitleCode = NULL, NameSuffixID = NULL, NameSuffixCode = NULL, LastName = NULL, FirstName = NULL, MiddleName = NULL, SocialSecurityNumber = NULL, MaskedSocialSecurityNumber = NULL, FederalEIN = NULL, BirthDate = NULL, GenderCode = NULL, IsAlaskan = NULL, IsAsian = NULL, IsBlack = NULL, IsHawaiian = NULL, IsHispanic = NULL, IsWhite = NULL, UserID = NULL, Username = NULL, IsActive = NULL, DirectionalID = NULL, DirectionalCode = NULL, ZipID = NULL, ZipCode = NULL, City = NULL, StateID = NULL, StateCode = NULL, StreetID = NULL, StreetName = NULL, AddressID = NULL, StreetNumber = NULL, AddressLine2 = NULL, ZipCodeAddOn = NULL, AddressTypeCode = NULL, CountryCode = NULL, BasicAddressDisplay = NULL, AddressSecondaryUnitID = NULL, AddressSecondaryUnitCode = NULL, SecondaryUnitNumber = NULL, EmployeeID = NULL, EmployeeNumber = NULL, AllowEmployeeAccess = NULL, W4Date = NULL, I9Date = NULL, CheckLocationID = NULL, CheckLocationCode = NULL, StartDate = NULL, HireDate = NULL, VendorID = NULL, VendorNumber = NULL, LineNumber = NULL, HasErrors = NULL, NamePhone1ID = NULL, PhoneNumber1 = NULL, PhoneType1ID = NULL, PhoneTypeCode1 = NULL, Extension1 = NULL, NamePhoneRank1 = NULL, NamePhone2ID = NULL, PhoneNumber2 = NULL, PhoneType2ID = NULL, PhoneTypeCode2 = NULL, Extension2 = NULL, NamePhoneRank2 = NULL, NamePhone3ID = NULL, PhoneNumber3 = NULL, PhoneType3ID = NULL, PhoneTypeCode3 = NULL, Extension3 = NULL, NamePhoneRank3 = NULL, NameEmail1ID = NULL, EmailAddress1 = NULL, EmailType1ID = NULL, EmailTypeCode1 = NULL, NameEmailRank1 = NULL, NameEmail2ID = NULL, EmailAddress2 = NULL, EmailType2ID = NULL, EmailTypeCode2 = NULL, NameEmailRank2 = NULL, NameEmail3ID = NULL, EmailAddress3 = NULL, EmailType3ID = NULL, EmailTypeCode3 = NULL, NameEmailRank3 = NULL, CreateNameAddress = NULL, IsUserName = NULL, EmployeeThirdPartyImportID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "TempEmployeeImportValueSet", body = list(DataObject = body), searchFields = append("TempEmployeeImportValueSetID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempEmployeeImportValueSet
	#'
	#' This function modifies a TempEmployeeImportValueSet
	#' @param fieldNames The field values to give the modified TempEmployeeImportValueSet. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified TempEmployeeImportValueSet
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempEmployeeImportValueSet <- function(TempEmployeeImportValueSetID, NameID = NULL, NameTitleID = NULL, NameTitleCode = NULL, NameSuffixID = NULL, NameSuffixCode = NULL, LastName = NULL, FirstName = NULL, MiddleName = NULL, SocialSecurityNumber = NULL, MaskedSocialSecurityNumber = NULL, FederalEIN = NULL, BirthDate = NULL, GenderCode = NULL, IsAlaskan = NULL, IsAsian = NULL, IsBlack = NULL, IsHawaiian = NULL, IsHispanic = NULL, IsWhite = NULL, UserID = NULL, Username = NULL, IsActive = NULL, DirectionalID = NULL, DirectionalCode = NULL, ZipID = NULL, ZipCode = NULL, City = NULL, StateID = NULL, StateCode = NULL, StreetID = NULL, StreetName = NULL, AddressID = NULL, StreetNumber = NULL, AddressLine2 = NULL, ZipCodeAddOn = NULL, AddressTypeCode = NULL, CountryCode = NULL, BasicAddressDisplay = NULL, AddressSecondaryUnitID = NULL, AddressSecondaryUnitCode = NULL, SecondaryUnitNumber = NULL, EmployeeID = NULL, EmployeeNumber = NULL, AllowEmployeeAccess = NULL, W4Date = NULL, I9Date = NULL, CheckLocationID = NULL, CheckLocationCode = NULL, StartDate = NULL, HireDate = NULL, VendorID = NULL, VendorNumber = NULL, LineNumber = NULL, HasErrors = NULL, NamePhone1ID = NULL, PhoneNumber1 = NULL, PhoneType1ID = NULL, PhoneTypeCode1 = NULL, Extension1 = NULL, NamePhoneRank1 = NULL, NamePhone2ID = NULL, PhoneNumber2 = NULL, PhoneType2ID = NULL, PhoneTypeCode2 = NULL, Extension2 = NULL, NamePhoneRank2 = NULL, NamePhone3ID = NULL, PhoneNumber3 = NULL, PhoneType3ID = NULL, PhoneTypeCode3 = NULL, Extension3 = NULL, NamePhoneRank3 = NULL, NameEmail1ID = NULL, EmailAddress1 = NULL, EmailType1ID = NULL, EmailTypeCode1 = NULL, NameEmailRank1 = NULL, NameEmail2ID = NULL, EmailAddress2 = NULL, EmailType2ID = NULL, EmailTypeCode2 = NULL, NameEmailRank2 = NULL, NameEmail3ID = NULL, EmailAddress3 = NULL, EmailType3ID = NULL, EmailTypeCode3 = NULL, NameEmailRank3 = NULL, CreateNameAddress = NULL, IsUserName = NULL, EmployeeThirdPartyImportID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "TempEmployeeImportValueSet", objectId = TempEmployeeImportValueSetID, body = list(DataObject = body), searchFields = append("TempEmployeeImportValueSetID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempWorkflowErrors
	#'
	#' This function returns a dataframe or json object of TempWorkflowErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempWorkflowErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempWorkflowErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempWorkflowError') to get more field paths.
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
	#' @concept Employee
	#' @return A list of TempWorkflowErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempWorkflowErrors <- function(searchConditionsList = NULL, TempWorkflowErrorID = F, TempObjectID = F, ErroredObjectID = F, Message = F, IsWarning = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ErroredObjectName = F, LineNumber = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "TempWorkflowError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempWorkflowError
	#'
	#' This function returns a dataframe or json object of a TempWorkflowError
	#' @param TempWorkflowErrorID The ID of the TempWorkflowError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempWorkflowError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempWorkflowError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempWorkflowError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of TempWorkflowError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempWorkflowError <- function(TempWorkflowErrorID, TempObjectID = F, ErroredObjectID = F, Message = F, IsWarning = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ErroredObjectName = F, LineNumber = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempWorkflowErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "TempWorkflowError", objectId = TempWorkflowErrorID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempWorkflowError
	#'
	#' This function deletes a TempWorkflowError
	#' @param TempWorkflowErrorID The ID of the TempWorkflowError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The TempWorkflowErrorID of the deleted TempWorkflowError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempWorkflowError <- function(TempWorkflowErrorID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "TempWorkflowError", objectId = TempWorkflowErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempWorkflowError
	#'
	#' This function creates a TempWorkflowError
	#' @param fieldNames The field values to give the created TempWorkflowError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created TempWorkflowError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempWorkflowError <- function(TempObjectID = NULL, ErroredObjectID = NULL, Message = NULL, IsWarning = NULL, ErroredObjectName = NULL, LineNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "TempWorkflowError", body = list(DataObject = body), searchFields = append("TempWorkflowErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempWorkflowError
	#'
	#' This function modifies a TempWorkflowError
	#' @param fieldNames The field values to give the modified TempWorkflowError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified TempWorkflowError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempWorkflowError <- function(TempWorkflowErrorID, TempObjectID = NULL, ErroredObjectID = NULL, Message = NULL, IsWarning = NULL, ErroredObjectName = NULL, LineNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "TempWorkflowError", objectId = TempWorkflowErrorID, body = list(DataObject = body), searchFields = append("TempWorkflowErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempCalendarDays
	#'
	#' This function returns a dataframe or json object of TempCalendarDays
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCalendarDays. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCalendarDays.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCalendarDay') to get more field paths.
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
	#' @concept Employee
	#' @return A list of TempCalendarDays
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempCalendarDays <- function(searchConditionsList = NULL, TempCalendarDayID = F, CalendarDayID = F, CalendarID = F, Date = F, IsWorkday = F, IsPaid = F, OverrideType = F, Comment = F, OverridePercent = F, OverrideSeconds = F, ErrorCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OldIsWorkday = F, OldIsPaid = F, OldOverrideType = F, OldComment = F, OldOverridePercent = F, OldOverrideSeconds = F, SaveTypeDisplay = F, SaveType = F, OldAdditionalSeconds = F, AdditionalSeconds = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "TempCalendarDay", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempCalendarDay
	#'
	#' This function returns a dataframe or json object of a TempCalendarDay
	#' @param TempCalendarDayID The ID of the TempCalendarDay to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCalendarDay. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCalendarDay.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCalendarDay') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of TempCalendarDay
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempCalendarDay <- function(TempCalendarDayID, CalendarDayID = F, CalendarID = F, Date = F, IsWorkday = F, IsPaid = F, OverrideType = F, Comment = F, OverridePercent = F, OverrideSeconds = F, ErrorCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OldIsWorkday = F, OldIsPaid = F, OldOverrideType = F, OldComment = F, OldOverridePercent = F, OldOverrideSeconds = F, SaveTypeDisplay = F, SaveType = F, OldAdditionalSeconds = F, AdditionalSeconds = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempCalendarDayID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "TempCalendarDay", objectId = TempCalendarDayID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempCalendarDay
	#'
	#' This function deletes a TempCalendarDay
	#' @param TempCalendarDayID The ID of the TempCalendarDay to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The TempCalendarDayID of the deleted TempCalendarDay.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempCalendarDay <- function(TempCalendarDayID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "TempCalendarDay", objectId = TempCalendarDayID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempCalendarDay
	#'
	#' This function creates a TempCalendarDay
	#' @param fieldNames The field values to give the created TempCalendarDay. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created TempCalendarDay
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempCalendarDay <- function(CalendarDayID = NULL, CalendarID = NULL, Date = NULL, IsWorkday = NULL, IsPaid = NULL, OverrideType = NULL, Comment = NULL, OverridePercent = NULL, OverrideSeconds = NULL, ErrorCount = NULL, OldIsWorkday = NULL, OldIsPaid = NULL, OldOverrideType = NULL, OldComment = NULL, OldOverridePercent = NULL, OldOverrideSeconds = NULL, OldAdditionalSeconds = NULL, AdditionalSeconds = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "TempCalendarDay", body = list(DataObject = body), searchFields = append("TempCalendarDayID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempCalendarDay
	#'
	#' This function modifies a TempCalendarDay
	#' @param fieldNames The field values to give the modified TempCalendarDay. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified TempCalendarDay
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempCalendarDay <- function(TempCalendarDayID, CalendarDayID = NULL, CalendarID = NULL, Date = NULL, IsWorkday = NULL, IsPaid = NULL, OverrideType = NULL, Comment = NULL, OverridePercent = NULL, OverrideSeconds = NULL, ErrorCount = NULL, OldIsWorkday = NULL, OldIsPaid = NULL, OldOverrideType = NULL, OldComment = NULL, OldOverridePercent = NULL, OldOverrideSeconds = NULL, OldAdditionalSeconds = NULL, AdditionalSeconds = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "TempCalendarDay", objectId = TempCalendarDayID, body = list(DataObject = body), searchFields = append("TempCalendarDayID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempImportErrors
	#'
	#' This function returns a dataframe or json object of TempImportErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempImportErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempImportErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempImportError') to get more field paths.
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
	#' @concept Employee
	#' @return A list of TempImportErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempImportErrors <- function(searchConditionsList = NULL, TempImportErrorID = F, Message = F, LineNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TempObjectID = F, IsFatal = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "TempImportError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempImportError
	#'
	#' This function returns a dataframe or json object of a TempImportError
	#' @param TempImportErrorID The ID of the TempImportError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempImportError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempImportError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempImportError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of TempImportError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempImportError <- function(TempImportErrorID, Message = F, LineNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TempObjectID = F, IsFatal = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempImportErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "TempImportError", objectId = TempImportErrorID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempImportError
	#'
	#' This function deletes a TempImportError
	#' @param TempImportErrorID The ID of the TempImportError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The TempImportErrorID of the deleted TempImportError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempImportError <- function(TempImportErrorID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "TempImportError", objectId = TempImportErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempImportError
	#'
	#' This function creates a TempImportError
	#' @param fieldNames The field values to give the created TempImportError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created TempImportError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempImportError <- function(Message = NULL, LineNumber = NULL, TempObjectID = NULL, IsFatal = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "TempImportError", body = list(DataObject = body), searchFields = append("TempImportErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempImportError
	#'
	#' This function modifies a TempImportError
	#' @param fieldNames The field values to give the modified TempImportError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified TempImportError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempImportError <- function(TempImportErrorID, Message = NULL, LineNumber = NULL, TempObjectID = NULL, IsFatal = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "TempImportError", objectId = TempImportErrorID, body = list(DataObject = body), searchFields = append("TempImportErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempWorkflowErrorGroupings
	#'
	#' This function returns a dataframe or json object of TempWorkflowErrorGroupings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempWorkflowErrorGroupings. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempWorkflowErrorGroupings.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempWorkflowErrorGrouping') to get more field paths.
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
	#' @concept Employee
	#' @return A list of TempWorkflowErrorGroupings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempWorkflowErrorGroupings <- function(searchConditionsList = NULL, TempWorkflowErrorGroupingID = F, ErroredObjectID = F, ErroredObjectName = F, Identifier = F, ErrorCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "TempWorkflowErrorGrouping", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempWorkflowErrorGrouping
	#'
	#' This function returns a dataframe or json object of a TempWorkflowErrorGrouping
	#' @param TempWorkflowErrorGroupingID The ID of the TempWorkflowErrorGrouping to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempWorkflowErrorGrouping. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempWorkflowErrorGrouping.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempWorkflowErrorGrouping') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of TempWorkflowErrorGrouping
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempWorkflowErrorGrouping <- function(TempWorkflowErrorGroupingID, ErroredObjectID = F, ErroredObjectName = F, Identifier = F, ErrorCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempWorkflowErrorGroupingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "TempWorkflowErrorGrouping", objectId = TempWorkflowErrorGroupingID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempWorkflowErrorGrouping
	#'
	#' This function deletes a TempWorkflowErrorGrouping
	#' @param TempWorkflowErrorGroupingID The ID of the TempWorkflowErrorGrouping to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The TempWorkflowErrorGroupingID of the deleted TempWorkflowErrorGrouping.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempWorkflowErrorGrouping <- function(TempWorkflowErrorGroupingID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "TempWorkflowErrorGrouping", objectId = TempWorkflowErrorGroupingID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempWorkflowErrorGrouping
	#'
	#' This function creates a TempWorkflowErrorGrouping
	#' @param fieldNames The field values to give the created TempWorkflowErrorGrouping. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created TempWorkflowErrorGrouping
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempWorkflowErrorGrouping <- function(ErroredObjectID = NULL, ErroredObjectName = NULL, Identifier = NULL, ErrorCount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "TempWorkflowErrorGrouping", body = list(DataObject = body), searchFields = append("TempWorkflowErrorGroupingID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempWorkflowErrorGrouping
	#'
	#' This function modifies a TempWorkflowErrorGrouping
	#' @param fieldNames The field values to give the modified TempWorkflowErrorGrouping. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified TempWorkflowErrorGrouping
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempWorkflowErrorGrouping <- function(TempWorkflowErrorGroupingID, ErroredObjectID = NULL, ErroredObjectName = NULL, Identifier = NULL, ErrorCount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "TempWorkflowErrorGrouping", objectId = TempWorkflowErrorGroupingID, body = list(DataObject = body), searchFields = append("TempWorkflowErrorGroupingID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EmployeeRetirementStatusMNS
	#'
	#' This function returns a dataframe or json object of EmployeeRetirementStatusMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeRetirementStatusMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeRetirementStatusMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeRetirementStatusMN') to get more field paths.
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
	#' @concept Employee
	#' @return A list of EmployeeRetirementStatusMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEmployeeRetirementStatusMNS <- function(searchConditionsList = NULL, EmployeeRetirementStatusMNID = F, EmployeeRetirementMNID = F, StartDate = F, EndDate = F, StatePERAMemberEmploymentStatusMNID = F, StateTRAStatusMNID = F, StatePERAMemberEmploymentStatusMNIDTermination = F, StateTRAStatusMNIDTermination = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsActive = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "EmployeeRetirementStatusMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EmployeeRetirementStatusMN
	#'
	#' This function returns a dataframe or json object of an EmployeeRetirementStatusMN
	#' @param EmployeeRetirementStatusMNID The ID of the EmployeeRetirementStatusMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeeRetirementStatusMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeeRetirementStatusMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeeRetirementStatusMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of EmployeeRetirementStatusMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEmployeeRetirementStatusMN <- function(EmployeeRetirementStatusMNID, EmployeeRetirementMNID = F, StartDate = F, EndDate = F, StatePERAMemberEmploymentStatusMNID = F, StateTRAStatusMNID = F, StatePERAMemberEmploymentStatusMNIDTermination = F, StateTRAStatusMNIDTermination = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsActive = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EmployeeRetirementStatusMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "EmployeeRetirementStatusMN", objectId = EmployeeRetirementStatusMNID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EmployeeRetirementStatusMN
	#'
	#' This function deletes an EmployeeRetirementStatusMN
	#' @param EmployeeRetirementStatusMNID The ID of the EmployeeRetirementStatusMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The EmployeeRetirementStatusMNID of the deleted EmployeeRetirementStatusMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEmployeeRetirementStatusMN <- function(EmployeeRetirementStatusMNID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "EmployeeRetirementStatusMN", objectId = EmployeeRetirementStatusMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EmployeeRetirementStatusMN
	#'
	#' This function creates an EmployeeRetirementStatusMN
	#' @param fieldNames The field values to give the created EmployeeRetirementStatusMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created EmployeeRetirementStatusMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEmployeeRetirementStatusMN <- function(EmployeeRetirementMNID = NULL, StartDate = NULL, EndDate = NULL, StatePERAMemberEmploymentStatusMNID = NULL, StateTRAStatusMNID = NULL, StatePERAMemberEmploymentStatusMNIDTermination = NULL, StateTRAStatusMNIDTermination = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "EmployeeRetirementStatusMN", body = list(DataObject = body), searchFields = append("EmployeeRetirementStatusMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EmployeeRetirementStatusMN
	#'
	#' This function modifies an EmployeeRetirementStatusMN
	#' @param fieldNames The field values to give the modified EmployeeRetirementStatusMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified EmployeeRetirementStatusMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEmployeeRetirementStatusMN <- function(EmployeeRetirementStatusMNID, EmployeeRetirementMNID = NULL, StartDate = NULL, EndDate = NULL, StatePERAMemberEmploymentStatusMNID = NULL, StateTRAStatusMNID = NULL, StatePERAMemberEmploymentStatusMNIDTermination = NULL, StateTRAStatusMNIDTermination = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "EmployeeRetirementStatusMN", objectId = EmployeeRetirementStatusMNID, body = list(DataObject = body), searchFields = append("EmployeeRetirementStatusMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempEmployeeACAOfferAndCoverages
	#'
	#' This function returns a dataframe or json object of TempEmployeeACAOfferAndCoverages
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempEmployeeACAOfferAndCoverages. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempEmployeeACAOfferAndCoverages.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempEmployeeACAOfferAndCoverage') to get more field paths.
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
	#' @concept Employee
	#' @return A list of TempEmployeeACAOfferAndCoverages
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempEmployeeACAOfferAndCoverages <- function(searchConditionsList = NULL, TempEmployeeACAOfferAndCoverageID = F, EmployeeACAOfferAndCoverageID = F, EmployeeID = F, EmployeeNameLFM = F, EmployeeNumber = F, FederalACAOfferAndCoverageID = F, FederalACAOfferAndCoverageCode = F, ACAEmployeeRequiredContribution = F, FederalACASafeHarborID = F, FederalACASafeHarborCode = F, EmployerOfferedSelfInsuredCoverage = F, ReportEmployeeAsCoveredIndividual = F, Comment = F, ErrorCount = F, HasErrors = F, Month = F, Year = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IncludeInACA = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "TempEmployeeACAOfferAndCoverage", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempEmployeeACAOfferAndCoverage
	#'
	#' This function returns a dataframe or json object of a TempEmployeeACAOfferAndCoverage
	#' @param TempEmployeeACAOfferAndCoverageID The ID of the TempEmployeeACAOfferAndCoverage to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempEmployeeACAOfferAndCoverage. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempEmployeeACAOfferAndCoverage.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempEmployeeACAOfferAndCoverage') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of TempEmployeeACAOfferAndCoverage
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempEmployeeACAOfferAndCoverage <- function(TempEmployeeACAOfferAndCoverageID, EmployeeACAOfferAndCoverageID = F, EmployeeID = F, EmployeeNameLFM = F, EmployeeNumber = F, FederalACAOfferAndCoverageID = F, FederalACAOfferAndCoverageCode = F, ACAEmployeeRequiredContribution = F, FederalACASafeHarborID = F, FederalACASafeHarborCode = F, EmployerOfferedSelfInsuredCoverage = F, ReportEmployeeAsCoveredIndividual = F, Comment = F, ErrorCount = F, HasErrors = F, Month = F, Year = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IncludeInACA = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempEmployeeACAOfferAndCoverageID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "TempEmployeeACAOfferAndCoverage", objectId = TempEmployeeACAOfferAndCoverageID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempEmployeeACAOfferAndCoverage
	#'
	#' This function deletes a TempEmployeeACAOfferAndCoverage
	#' @param TempEmployeeACAOfferAndCoverageID The ID of the TempEmployeeACAOfferAndCoverage to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The TempEmployeeACAOfferAndCoverageID of the deleted TempEmployeeACAOfferAndCoverage.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempEmployeeACAOfferAndCoverage <- function(TempEmployeeACAOfferAndCoverageID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "TempEmployeeACAOfferAndCoverage", objectId = TempEmployeeACAOfferAndCoverageID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempEmployeeACAOfferAndCoverage
	#'
	#' This function creates a TempEmployeeACAOfferAndCoverage
	#' @param fieldNames The field values to give the created TempEmployeeACAOfferAndCoverage. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created TempEmployeeACAOfferAndCoverage
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempEmployeeACAOfferAndCoverage <- function(EmployeeACAOfferAndCoverageID = NULL, EmployeeID = NULL, EmployeeNameLFM = NULL, EmployeeNumber = NULL, FederalACAOfferAndCoverageID = NULL, FederalACAOfferAndCoverageCode = NULL, ACAEmployeeRequiredContribution = NULL, FederalACASafeHarborID = NULL, FederalACASafeHarborCode = NULL, EmployerOfferedSelfInsuredCoverage = NULL, ReportEmployeeAsCoveredIndividual = NULL, Comment = NULL, ErrorCount = NULL, HasErrors = NULL, Month = NULL, Year = NULL, IncludeInACA = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "TempEmployeeACAOfferAndCoverage", body = list(DataObject = body), searchFields = append("TempEmployeeACAOfferAndCoverageID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempEmployeeACAOfferAndCoverage
	#'
	#' This function modifies a TempEmployeeACAOfferAndCoverage
	#' @param fieldNames The field values to give the modified TempEmployeeACAOfferAndCoverage. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified TempEmployeeACAOfferAndCoverage
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempEmployeeACAOfferAndCoverage <- function(TempEmployeeACAOfferAndCoverageID, EmployeeACAOfferAndCoverageID = NULL, EmployeeID = NULL, EmployeeNameLFM = NULL, EmployeeNumber = NULL, FederalACAOfferAndCoverageID = NULL, FederalACAOfferAndCoverageCode = NULL, ACAEmployeeRequiredContribution = NULL, FederalACASafeHarborID = NULL, FederalACASafeHarborCode = NULL, EmployerOfferedSelfInsuredCoverage = NULL, ReportEmployeeAsCoveredIndividual = NULL, Comment = NULL, ErrorCount = NULL, HasErrors = NULL, Month = NULL, Year = NULL, IncludeInACA = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "TempEmployeeACAOfferAndCoverage", objectId = TempEmployeeACAOfferAndCoverageID, body = list(DataObject = body), searchFields = append("TempEmployeeACAOfferAndCoverageID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempEmployeeACAOfferAndCoverageDependents
	#'
	#' This function returns a dataframe or json object of TempEmployeeACAOfferAndCoverageDependents
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempEmployeeACAOfferAndCoverageDependents. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempEmployeeACAOfferAndCoverageDependents.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempEmployeeACAOfferAndCoverageDependent') to get more field paths.
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
	#' @concept Employee
	#' @return A list of TempEmployeeACAOfferAndCoverageDependents
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempEmployeeACAOfferAndCoverageDependents <- function(searchConditionsList = NULL, TempEmployeeACAOfferAndCoverageDependentID = F, EmployeeACAOfferAndCoverageID = F, EmployeeID = F, EmployeeNameID = F, EmployeeNameLFM = F, EmployeeNumber = F, LastName = F, FirstName = F, MiddleName = F, SSN = F, MaskedSSN = F, BirthDate = F, ErrorCount = F, HasErrors = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DependentID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "TempEmployeeACAOfferAndCoverageDependent", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempEmployeeACAOfferAndCoverageDependent
	#'
	#' This function returns a dataframe or json object of a TempEmployeeACAOfferAndCoverageDependent
	#' @param TempEmployeeACAOfferAndCoverageDependentID The ID of the TempEmployeeACAOfferAndCoverageDependent to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempEmployeeACAOfferAndCoverageDependent. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempEmployeeACAOfferAndCoverageDependent.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempEmployeeACAOfferAndCoverageDependent') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of TempEmployeeACAOfferAndCoverageDependent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempEmployeeACAOfferAndCoverageDependent <- function(TempEmployeeACAOfferAndCoverageDependentID, EmployeeACAOfferAndCoverageID = F, EmployeeID = F, EmployeeNameID = F, EmployeeNameLFM = F, EmployeeNumber = F, LastName = F, FirstName = F, MiddleName = F, SSN = F, MaskedSSN = F, BirthDate = F, ErrorCount = F, HasErrors = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DependentID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempEmployeeACAOfferAndCoverageDependentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "TempEmployeeACAOfferAndCoverageDependent", objectId = TempEmployeeACAOfferAndCoverageDependentID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempEmployeeACAOfferAndCoverageDependent
	#'
	#' This function deletes a TempEmployeeACAOfferAndCoverageDependent
	#' @param TempEmployeeACAOfferAndCoverageDependentID The ID of the TempEmployeeACAOfferAndCoverageDependent to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The TempEmployeeACAOfferAndCoverageDependentID of the deleted TempEmployeeACAOfferAndCoverageDependent.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempEmployeeACAOfferAndCoverageDependent <- function(TempEmployeeACAOfferAndCoverageDependentID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "TempEmployeeACAOfferAndCoverageDependent", objectId = TempEmployeeACAOfferAndCoverageDependentID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempEmployeeACAOfferAndCoverageDependent
	#'
	#' This function creates a TempEmployeeACAOfferAndCoverageDependent
	#' @param fieldNames The field values to give the created TempEmployeeACAOfferAndCoverageDependent. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created TempEmployeeACAOfferAndCoverageDependent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempEmployeeACAOfferAndCoverageDependent <- function(EmployeeACAOfferAndCoverageID = NULL, EmployeeID = NULL, EmployeeNameID = NULL, EmployeeNameLFM = NULL, EmployeeNumber = NULL, LastName = NULL, FirstName = NULL, MiddleName = NULL, SSN = NULL, MaskedSSN = NULL, BirthDate = NULL, ErrorCount = NULL, HasErrors = NULL, DependentID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "TempEmployeeACAOfferAndCoverageDependent", body = list(DataObject = body), searchFields = append("TempEmployeeACAOfferAndCoverageDependentID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempEmployeeACAOfferAndCoverageDependent
	#'
	#' This function modifies a TempEmployeeACAOfferAndCoverageDependent
	#' @param fieldNames The field values to give the modified TempEmployeeACAOfferAndCoverageDependent. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified TempEmployeeACAOfferAndCoverageDependent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempEmployeeACAOfferAndCoverageDependent <- function(TempEmployeeACAOfferAndCoverageDependentID, EmployeeACAOfferAndCoverageID = NULL, EmployeeID = NULL, EmployeeNameID = NULL, EmployeeNameLFM = NULL, EmployeeNumber = NULL, LastName = NULL, FirstName = NULL, MiddleName = NULL, SSN = NULL, MaskedSSN = NULL, BirthDate = NULL, ErrorCount = NULL, HasErrors = NULL, DependentID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "TempEmployeeACAOfferAndCoverageDependent", objectId = TempEmployeeACAOfferAndCoverageDependentID, body = list(DataObject = body), searchFields = append("TempEmployeeACAOfferAndCoverageDependentID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempEmployeeACAOfferAndCoverageErrorDetails
	#'
	#' This function returns a dataframe or json object of TempEmployeeACAOfferAndCoverageErrorDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempEmployeeACAOfferAndCoverageErrorDetails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempEmployeeACAOfferAndCoverageErrorDetails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempEmployeeACAOfferAndCoverageErrorDetail') to get more field paths.
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
	#' @concept Employee
	#' @return A list of TempEmployeeACAOfferAndCoverageErrorDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempEmployeeACAOfferAndCoverageErrorDetails <- function(searchConditionsList = NULL, TempEmployeeACAOfferAndCoverageErrorDetailID = F, TempEmployeeACAOfferAndCoverageID = F, TempEmployeeACAOfferAndCoverageDependentID = F, Error = F, ErrorDetail = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Employee", objectName = "TempEmployeeACAOfferAndCoverageErrorDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempEmployeeACAOfferAndCoverageErrorDetail
	#'
	#' This function returns a dataframe or json object of a TempEmployeeACAOfferAndCoverageErrorDetail
	#' @param TempEmployeeACAOfferAndCoverageErrorDetailID The ID of the TempEmployeeACAOfferAndCoverageErrorDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempEmployeeACAOfferAndCoverageErrorDetail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempEmployeeACAOfferAndCoverageErrorDetail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempEmployeeACAOfferAndCoverageErrorDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A dataframe or of TempEmployeeACAOfferAndCoverageErrorDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempEmployeeACAOfferAndCoverageErrorDetail <- function(TempEmployeeACAOfferAndCoverageErrorDetailID, TempEmployeeACAOfferAndCoverageID = F, TempEmployeeACAOfferAndCoverageDependentID = F, Error = F, ErrorDetail = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempEmployeeACAOfferAndCoverageErrorDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Employee", objectName = "TempEmployeeACAOfferAndCoverageErrorDetail", objectId = TempEmployeeACAOfferAndCoverageErrorDetailID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempEmployeeACAOfferAndCoverageErrorDetail
	#'
	#' This function deletes a TempEmployeeACAOfferAndCoverageErrorDetail
	#' @param TempEmployeeACAOfferAndCoverageErrorDetailID The ID of the TempEmployeeACAOfferAndCoverageErrorDetail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The TempEmployeeACAOfferAndCoverageErrorDetailID of the deleted TempEmployeeACAOfferAndCoverageErrorDetail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempEmployeeACAOfferAndCoverageErrorDetail <- function(TempEmployeeACAOfferAndCoverageErrorDetailID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Employee", objectName = "TempEmployeeACAOfferAndCoverageErrorDetail", objectId = TempEmployeeACAOfferAndCoverageErrorDetailID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempEmployeeACAOfferAndCoverageErrorDetail
	#'
	#' This function creates a TempEmployeeACAOfferAndCoverageErrorDetail
	#' @param fieldNames The field values to give the created TempEmployeeACAOfferAndCoverageErrorDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return A newly created TempEmployeeACAOfferAndCoverageErrorDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempEmployeeACAOfferAndCoverageErrorDetail <- function(TempEmployeeACAOfferAndCoverageID = NULL, TempEmployeeACAOfferAndCoverageDependentID = NULL, Error = NULL, ErrorDetail = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "Employee", objectName = "TempEmployeeACAOfferAndCoverageErrorDetail", body = list(DataObject = body), searchFields = append("TempEmployeeACAOfferAndCoverageErrorDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempEmployeeACAOfferAndCoverageErrorDetail
	#'
	#' This function modifies a TempEmployeeACAOfferAndCoverageErrorDetail
	#' @param fieldNames The field values to give the modified TempEmployeeACAOfferAndCoverageErrorDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Employee
	#' @return The modified TempEmployeeACAOfferAndCoverageErrorDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempEmployeeACAOfferAndCoverageErrorDetail <- function(TempEmployeeACAOfferAndCoverageErrorDetailID, TempEmployeeACAOfferAndCoverageID = NULL, TempEmployeeACAOfferAndCoverageDependentID = NULL, Error = NULL, ErrorDetail = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "Employee", objectName = "TempEmployeeACAOfferAndCoverageErrorDetail", objectId = TempEmployeeACAOfferAndCoverageErrorDetailID, body = list(DataObject = body), searchFields = append("TempEmployeeACAOfferAndCoverageErrorDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}
