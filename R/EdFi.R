
	#' List EdFiEdFiCourseLevelCharacteristics
	#'
	#' This function returns a dataframe or json object of EdFiEdFiCourseLevelCharacteristics
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EdFiEdFiCourseLevelCharacteristics. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EdFiEdFiCourseLevelCharacteristics.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EdFiEdFiCourseLevelCharacteristic') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return A list of EdFiEdFiCourseLevelCharacteristics
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEdFiEdFiCourseLevelCharacteristics <- function(searchConditionsList = NULL, EdFiCourseLevelCharacteristicID = F, CodeValue = F, Namespace = F, Description = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "EdFi", objectName = "EdFiCourseLevelCharacteristic", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EdFiEdFiCourseLevelCharacteristic
	#'
	#' This function returns a dataframe or json object of an EdFiEdFiCourseLevelCharacteristic
	#' @param EdFiEdFiCourseLevelCharacteristicID The ID of the EdFiEdFiCourseLevelCharacteristic to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EdFiEdFiCourseLevelCharacteristic. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EdFiEdFiCourseLevelCharacteristic.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EdFiEdFiCourseLevelCharacteristic') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return A dataframe or of EdFiEdFiCourseLevelCharacteristic
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEdFiEdFiCourseLevelCharacteristic <- function(EdFiEdFiCourseLevelCharacteristicID, EdFiCourseLevelCharacteristicID = F, CodeValue = F, Namespace = F, Description = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EdFiEdFiCourseLevelCharacteristicID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "EdFi", objectName = "EdFiCourseLevelCharacteristic", objectId = EdFiEdFiCourseLevelCharacteristicID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EdFiEdFiCourseLevelCharacteristic
	#'
	#' This function deletes an EdFiEdFiCourseLevelCharacteristic
	#' @param EdFiEdFiCourseLevelCharacteristicID The ID of the EdFiEdFiCourseLevelCharacteristic to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return The EdFiEdFiCourseLevelCharacteristicID of the deleted EdFiEdFiCourseLevelCharacteristic.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEdFiEdFiCourseLevelCharacteristic <- function(EdFiEdFiCourseLevelCharacteristicID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "EdFi", objectName = "EdFiCourseLevelCharacteristic", objectId = EdFiEdFiCourseLevelCharacteristicID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EdFiEdFiCourseLevelCharacteristic
	#'
	#' This function creates an EdFiEdFiCourseLevelCharacteristic
	#' @param fieldNames The field values to give the created EdFiEdFiCourseLevelCharacteristic. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return A newly created EdFiEdFiCourseLevelCharacteristic
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEdFiEdFiCourseLevelCharacteristic <- function(CodeValue = NULL, Namespace = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "EdFi", objectName = "EdFiCourseLevelCharacteristic", body = list(DataObject = body), searchFields = append("EdFiCourseLevelCharacteristicID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EdFiEdFiCourseLevelCharacteristic
	#'
	#' This function modifies an EdFiEdFiCourseLevelCharacteristic
	#' @param fieldNames The field values to give the modified EdFiEdFiCourseLevelCharacteristic. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return The modified EdFiEdFiCourseLevelCharacteristic
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEdFiEdFiCourseLevelCharacteristic <- function(EdFiCourseLevelCharacteristicID, CodeValue = NULL, Namespace = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "EdFi", objectName = "EdFiCourseLevelCharacteristic", objectId = EdFiCourseLevelCharacteristicID, body = list(DataObject = body), searchFields = append("EdFiCourseLevelCharacteristicID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EdFiDisciplineDescriptors
	#'
	#' This function returns a dataframe or json object of EdFiDisciplineDescriptors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EdFiDisciplineDescriptors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EdFiDisciplineDescriptors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EdFiDisciplineDescriptor') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return A list of EdFiDisciplineDescriptors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEdFiDisciplineDescriptors <- function(searchConditionsList = NULL, EdFiDisciplineDescriptorID = F, Code = F, Description = F, CodeDescription = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "EdFi", objectName = "EdFiDisciplineDescriptor", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EdFiDisciplineDescriptor
	#'
	#' This function returns a dataframe or json object of an EdFiDisciplineDescriptor
	#' @param EdFiDisciplineDescriptorID The ID of the EdFiDisciplineDescriptor to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EdFiDisciplineDescriptor. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EdFiDisciplineDescriptor.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EdFiDisciplineDescriptor') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return A dataframe or of EdFiDisciplineDescriptor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEdFiDisciplineDescriptor <- function(EdFiDisciplineDescriptorID, Code = F, Description = F, CodeDescription = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EdFiDisciplineDescriptorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "EdFi", objectName = "EdFiDisciplineDescriptor", objectId = EdFiDisciplineDescriptorID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EdFiDisciplineDescriptor
	#'
	#' This function deletes an EdFiDisciplineDescriptor
	#' @param EdFiDisciplineDescriptorID The ID of the EdFiDisciplineDescriptor to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return The EdFiDisciplineDescriptorID of the deleted EdFiDisciplineDescriptor.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEdFiDisciplineDescriptor <- function(EdFiDisciplineDescriptorID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "EdFi", objectName = "EdFiDisciplineDescriptor", objectId = EdFiDisciplineDescriptorID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EdFiDisciplineDescriptor
	#'
	#' This function creates an EdFiDisciplineDescriptor
	#' @param fieldNames The field values to give the created EdFiDisciplineDescriptor. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return A newly created EdFiDisciplineDescriptor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEdFiDisciplineDescriptor <- function(Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "EdFi", objectName = "EdFiDisciplineDescriptor", body = list(DataObject = body), searchFields = append("EdFiDisciplineDescriptorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EdFiDisciplineDescriptor
	#'
	#' This function modifies an EdFiDisciplineDescriptor
	#' @param fieldNames The field values to give the modified EdFiDisciplineDescriptor. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return The modified EdFiDisciplineDescriptor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEdFiDisciplineDescriptor <- function(EdFiDisciplineDescriptorID, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "EdFi", objectName = "EdFiDisciplineDescriptor", objectId = EdFiDisciplineDescriptorID, body = list(DataObject = body), searchFields = append("EdFiDisciplineDescriptorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EdFiGraduationPlanTypeDescriptors
	#'
	#' This function returns a dataframe or json object of EdFiGraduationPlanTypeDescriptors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EdFiGraduationPlanTypeDescriptors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EdFiGraduationPlanTypeDescriptors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EdFiGraduationPlanTypeDescriptor') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return A list of EdFiGraduationPlanTypeDescriptors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEdFiGraduationPlanTypeDescriptors <- function(searchConditionsList = NULL, EdFiGraduationPlanTypeDescriptorID = F, Code = F, Description = F, CodeDescription = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "EdFi", objectName = "EdFiGraduationPlanTypeDescriptor", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EdFiGraduationPlanTypeDescriptor
	#'
	#' This function returns a dataframe or json object of an EdFiGraduationPlanTypeDescriptor
	#' @param EdFiGraduationPlanTypeDescriptorID The ID of the EdFiGraduationPlanTypeDescriptor to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EdFiGraduationPlanTypeDescriptor. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EdFiGraduationPlanTypeDescriptor.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EdFiGraduationPlanTypeDescriptor') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return A dataframe or of EdFiGraduationPlanTypeDescriptor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEdFiGraduationPlanTypeDescriptor <- function(EdFiGraduationPlanTypeDescriptorID, Code = F, Description = F, CodeDescription = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EdFiGraduationPlanTypeDescriptorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "EdFi", objectName = "EdFiGraduationPlanTypeDescriptor", objectId = EdFiGraduationPlanTypeDescriptorID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EdFiGraduationPlanTypeDescriptor
	#'
	#' This function deletes an EdFiGraduationPlanTypeDescriptor
	#' @param EdFiGraduationPlanTypeDescriptorID The ID of the EdFiGraduationPlanTypeDescriptor to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return The EdFiGraduationPlanTypeDescriptorID of the deleted EdFiGraduationPlanTypeDescriptor.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEdFiGraduationPlanTypeDescriptor <- function(EdFiGraduationPlanTypeDescriptorID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "EdFi", objectName = "EdFiGraduationPlanTypeDescriptor", objectId = EdFiGraduationPlanTypeDescriptorID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EdFiGraduationPlanTypeDescriptor
	#'
	#' This function creates an EdFiGraduationPlanTypeDescriptor
	#' @param fieldNames The field values to give the created EdFiGraduationPlanTypeDescriptor. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return A newly created EdFiGraduationPlanTypeDescriptor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEdFiGraduationPlanTypeDescriptor <- function(Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "EdFi", objectName = "EdFiGraduationPlanTypeDescriptor", body = list(DataObject = body), searchFields = append("EdFiGraduationPlanTypeDescriptorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EdFiGraduationPlanTypeDescriptor
	#'
	#' This function modifies an EdFiGraduationPlanTypeDescriptor
	#' @param fieldNames The field values to give the modified EdFiGraduationPlanTypeDescriptor. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return The modified EdFiGraduationPlanTypeDescriptor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEdFiGraduationPlanTypeDescriptor <- function(EdFiGraduationPlanTypeDescriptorID, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "EdFi", objectName = "EdFiGraduationPlanTypeDescriptor", objectId = EdFiGraduationPlanTypeDescriptorID, body = list(DataObject = body), searchFields = append("EdFiGraduationPlanTypeDescriptorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EdFiEdFiGradingPeriodDescriptors
	#'
	#' This function returns a dataframe or json object of EdFiEdFiGradingPeriodDescriptors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EdFiEdFiGradingPeriodDescriptors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EdFiEdFiGradingPeriodDescriptors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EdFiEdFiGradingPeriodDescriptor') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return A list of EdFiEdFiGradingPeriodDescriptors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEdFiEdFiGradingPeriodDescriptors <- function(searchConditionsList = NULL, EdFiGradingPeriodDescriptorID = F, Code = F, Description = F, CodeDescription = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "EdFi", objectName = "EdFiGradingPeriodDescriptor", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EdFiEdFiGradingPeriodDescriptor
	#'
	#' This function returns a dataframe or json object of an EdFiEdFiGradingPeriodDescriptor
	#' @param EdFiEdFiGradingPeriodDescriptorID The ID of the EdFiEdFiGradingPeriodDescriptor to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EdFiEdFiGradingPeriodDescriptor. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EdFiEdFiGradingPeriodDescriptor.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EdFiEdFiGradingPeriodDescriptor') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return A dataframe or of EdFiEdFiGradingPeriodDescriptor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEdFiEdFiGradingPeriodDescriptor <- function(EdFiEdFiGradingPeriodDescriptorID, EdFiGradingPeriodDescriptorID = F, Code = F, Description = F, CodeDescription = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EdFiEdFiGradingPeriodDescriptorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "EdFi", objectName = "EdFiGradingPeriodDescriptor", objectId = EdFiEdFiGradingPeriodDescriptorID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EdFiEdFiGradingPeriodDescriptor
	#'
	#' This function deletes an EdFiEdFiGradingPeriodDescriptor
	#' @param EdFiEdFiGradingPeriodDescriptorID The ID of the EdFiEdFiGradingPeriodDescriptor to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return The EdFiEdFiGradingPeriodDescriptorID of the deleted EdFiEdFiGradingPeriodDescriptor.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEdFiEdFiGradingPeriodDescriptor <- function(EdFiEdFiGradingPeriodDescriptorID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "EdFi", objectName = "EdFiGradingPeriodDescriptor", objectId = EdFiEdFiGradingPeriodDescriptorID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EdFiEdFiGradingPeriodDescriptor
	#'
	#' This function creates an EdFiEdFiGradingPeriodDescriptor
	#' @param fieldNames The field values to give the created EdFiEdFiGradingPeriodDescriptor. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return A newly created EdFiEdFiGradingPeriodDescriptor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEdFiEdFiGradingPeriodDescriptor <- function(Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "EdFi", objectName = "EdFiGradingPeriodDescriptor", body = list(DataObject = body), searchFields = append("EdFiGradingPeriodDescriptorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EdFiEdFiGradingPeriodDescriptor
	#'
	#' This function modifies an EdFiEdFiGradingPeriodDescriptor
	#' @param fieldNames The field values to give the modified EdFiEdFiGradingPeriodDescriptor. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return The modified EdFiEdFiGradingPeriodDescriptor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEdFiEdFiGradingPeriodDescriptor <- function(EdFiGradingPeriodDescriptorID, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "EdFi", objectName = "EdFiGradingPeriodDescriptor", objectId = EdFiGradingPeriodDescriptorID, body = list(DataObject = body), searchFields = append("EdFiGradingPeriodDescriptorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EdFiGradeTypeDescriptors
	#'
	#' This function returns a dataframe or json object of EdFiGradeTypeDescriptors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EdFiGradeTypeDescriptors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EdFiGradeTypeDescriptors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EdFiGradeTypeDescriptor') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return A list of EdFiGradeTypeDescriptors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEdFiGradeTypeDescriptors <- function(searchConditionsList = NULL, EdFiGradeTypeDescriptorID = F, Code = F, Description = F, CodeDescription = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "EdFi", objectName = "EdFiGradeTypeDescriptor", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EdFiGradeTypeDescriptor
	#'
	#' This function returns a dataframe or json object of an EdFiGradeTypeDescriptor
	#' @param EdFiGradeTypeDescriptorID The ID of the EdFiGradeTypeDescriptor to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EdFiGradeTypeDescriptor. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EdFiGradeTypeDescriptor.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EdFiGradeTypeDescriptor') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return A dataframe or of EdFiGradeTypeDescriptor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEdFiGradeTypeDescriptor <- function(EdFiGradeTypeDescriptorID, Code = F, Description = F, CodeDescription = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EdFiGradeTypeDescriptorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "EdFi", objectName = "EdFiGradeTypeDescriptor", objectId = EdFiGradeTypeDescriptorID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EdFiGradeTypeDescriptor
	#'
	#' This function deletes an EdFiGradeTypeDescriptor
	#' @param EdFiGradeTypeDescriptorID The ID of the EdFiGradeTypeDescriptor to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return The EdFiGradeTypeDescriptorID of the deleted EdFiGradeTypeDescriptor.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEdFiGradeTypeDescriptor <- function(EdFiGradeTypeDescriptorID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "EdFi", objectName = "EdFiGradeTypeDescriptor", objectId = EdFiGradeTypeDescriptorID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EdFiGradeTypeDescriptor
	#'
	#' This function creates an EdFiGradeTypeDescriptor
	#' @param fieldNames The field values to give the created EdFiGradeTypeDescriptor. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return A newly created EdFiGradeTypeDescriptor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEdFiGradeTypeDescriptor <- function(Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "EdFi", objectName = "EdFiGradeTypeDescriptor", body = list(DataObject = body), searchFields = append("EdFiGradeTypeDescriptorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EdFiGradeTypeDescriptor
	#'
	#' This function modifies an EdFiGradeTypeDescriptor
	#' @param fieldNames The field values to give the modified EdFiGradeTypeDescriptor. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return The modified EdFiGradeTypeDescriptor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEdFiGradeTypeDescriptor <- function(EdFiGradeTypeDescriptorID, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "EdFi", objectName = "EdFiGradeTypeDescriptor", objectId = EdFiGradeTypeDescriptorID, body = list(DataObject = body), searchFields = append("EdFiGradeTypeDescriptorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EdFiEducationalEnvironmentDescriptors
	#'
	#' This function returns a dataframe or json object of EdFiEducationalEnvironmentDescriptors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EdFiEducationalEnvironmentDescriptors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EdFiEducationalEnvironmentDescriptors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EdFiEducationalEnvironmentDescriptor') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return A list of EdFiEducationalEnvironmentDescriptors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEdFiEducationalEnvironmentDescriptors <- function(searchConditionsList = NULL, EdFiEducationalEnvironmentDescriptorID = F, Code = F, Description = F, CodeDescription = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "EdFi", objectName = "EdFiEducationalEnvironmentDescriptor", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EdFiEducationalEnvironmentDescriptor
	#'
	#' This function returns a dataframe or json object of an EdFiEducationalEnvironmentDescriptor
	#' @param EdFiEducationalEnvironmentDescriptorID The ID of the EdFiEducationalEnvironmentDescriptor to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EdFiEducationalEnvironmentDescriptor. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EdFiEducationalEnvironmentDescriptor.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EdFiEducationalEnvironmentDescriptor') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return A dataframe or of EdFiEducationalEnvironmentDescriptor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEdFiEducationalEnvironmentDescriptor <- function(EdFiEducationalEnvironmentDescriptorID, Code = F, Description = F, CodeDescription = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EdFiEducationalEnvironmentDescriptorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "EdFi", objectName = "EdFiEducationalEnvironmentDescriptor", objectId = EdFiEducationalEnvironmentDescriptorID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EdFiEducationalEnvironmentDescriptor
	#'
	#' This function deletes an EdFiEducationalEnvironmentDescriptor
	#' @param EdFiEducationalEnvironmentDescriptorID The ID of the EdFiEducationalEnvironmentDescriptor to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return The EdFiEducationalEnvironmentDescriptorID of the deleted EdFiEducationalEnvironmentDescriptor.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEdFiEducationalEnvironmentDescriptor <- function(EdFiEducationalEnvironmentDescriptorID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "EdFi", objectName = "EdFiEducationalEnvironmentDescriptor", objectId = EdFiEducationalEnvironmentDescriptorID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EdFiEducationalEnvironmentDescriptor
	#'
	#' This function creates an EdFiEducationalEnvironmentDescriptor
	#' @param fieldNames The field values to give the created EdFiEducationalEnvironmentDescriptor. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return A newly created EdFiEducationalEnvironmentDescriptor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEdFiEducationalEnvironmentDescriptor <- function(Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "EdFi", objectName = "EdFiEducationalEnvironmentDescriptor", body = list(DataObject = body), searchFields = append("EdFiEducationalEnvironmentDescriptorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EdFiEducationalEnvironmentDescriptor
	#'
	#' This function modifies an EdFiEducationalEnvironmentDescriptor
	#' @param fieldNames The field values to give the modified EdFiEducationalEnvironmentDescriptor. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return The modified EdFiEducationalEnvironmentDescriptor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEdFiEducationalEnvironmentDescriptor <- function(EdFiEducationalEnvironmentDescriptorID, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "EdFi", objectName = "EdFiEducationalEnvironmentDescriptor", objectId = EdFiEducationalEnvironmentDescriptorID, body = list(DataObject = body), searchFields = append("EdFiEducationalEnvironmentDescriptorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EdFiElectronicMailTypeDescriptors
	#'
	#' This function returns a dataframe or json object of EdFiElectronicMailTypeDescriptors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EdFiElectronicMailTypeDescriptors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EdFiElectronicMailTypeDescriptors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EdFiElectronicMailTypeDescriptor') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return A list of EdFiElectronicMailTypeDescriptors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEdFiElectronicMailTypeDescriptors <- function(searchConditionsList = NULL, EdFiElectronicMailTypeDescriptorID = F, Code = F, Description = F, CodeDescription = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "EdFi", objectName = "EdFiElectronicMailTypeDescriptor", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EdFiElectronicMailTypeDescriptor
	#'
	#' This function returns a dataframe or json object of an EdFiElectronicMailTypeDescriptor
	#' @param EdFiElectronicMailTypeDescriptorID The ID of the EdFiElectronicMailTypeDescriptor to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EdFiElectronicMailTypeDescriptor. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EdFiElectronicMailTypeDescriptor.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EdFiElectronicMailTypeDescriptor') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return A dataframe or of EdFiElectronicMailTypeDescriptor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEdFiElectronicMailTypeDescriptor <- function(EdFiElectronicMailTypeDescriptorID, Code = F, Description = F, CodeDescription = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EdFiElectronicMailTypeDescriptorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "EdFi", objectName = "EdFiElectronicMailTypeDescriptor", objectId = EdFiElectronicMailTypeDescriptorID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EdFiElectronicMailTypeDescriptor
	#'
	#' This function deletes an EdFiElectronicMailTypeDescriptor
	#' @param EdFiElectronicMailTypeDescriptorID The ID of the EdFiElectronicMailTypeDescriptor to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return The EdFiElectronicMailTypeDescriptorID of the deleted EdFiElectronicMailTypeDescriptor.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEdFiElectronicMailTypeDescriptor <- function(EdFiElectronicMailTypeDescriptorID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "EdFi", objectName = "EdFiElectronicMailTypeDescriptor", objectId = EdFiElectronicMailTypeDescriptorID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EdFiElectronicMailTypeDescriptor
	#'
	#' This function creates an EdFiElectronicMailTypeDescriptor
	#' @param fieldNames The field values to give the created EdFiElectronicMailTypeDescriptor. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return A newly created EdFiElectronicMailTypeDescriptor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEdFiElectronicMailTypeDescriptor <- function(Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "EdFi", objectName = "EdFiElectronicMailTypeDescriptor", body = list(DataObject = body), searchFields = append("EdFiElectronicMailTypeDescriptorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EdFiElectronicMailTypeDescriptor
	#'
	#' This function modifies an EdFiElectronicMailTypeDescriptor
	#' @param fieldNames The field values to give the modified EdFiElectronicMailTypeDescriptor. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return The modified EdFiElectronicMailTypeDescriptor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEdFiElectronicMailTypeDescriptor <- function(EdFiElectronicMailTypeDescriptorID, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "EdFi", objectName = "EdFiElectronicMailTypeDescriptor", objectId = EdFiElectronicMailTypeDescriptorID, body = list(DataObject = body), searchFields = append("EdFiElectronicMailTypeDescriptorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EdFiEntryTypeDescriptors
	#'
	#' This function returns a dataframe or json object of EdFiEntryTypeDescriptors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EdFiEntryTypeDescriptors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EdFiEntryTypeDescriptors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EdFiEntryTypeDescriptor') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return A list of EdFiEntryTypeDescriptors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEdFiEntryTypeDescriptors <- function(searchConditionsList = NULL, EdFiEntryTypeDescriptorID = F, Code = F, Description = F, CodeDescription = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "EdFi", objectName = "EdFiEntryTypeDescriptor", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EdFiEntryTypeDescriptor
	#'
	#' This function returns a dataframe or json object of an EdFiEntryTypeDescriptor
	#' @param EdFiEntryTypeDescriptorID The ID of the EdFiEntryTypeDescriptor to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EdFiEntryTypeDescriptor. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EdFiEntryTypeDescriptor.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EdFiEntryTypeDescriptor') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return A dataframe or of EdFiEntryTypeDescriptor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEdFiEntryTypeDescriptor <- function(EdFiEntryTypeDescriptorID, Code = F, Description = F, CodeDescription = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EdFiEntryTypeDescriptorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "EdFi", objectName = "EdFiEntryTypeDescriptor", objectId = EdFiEntryTypeDescriptorID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EdFiEntryTypeDescriptor
	#'
	#' This function deletes an EdFiEntryTypeDescriptor
	#' @param EdFiEntryTypeDescriptorID The ID of the EdFiEntryTypeDescriptor to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return The EdFiEntryTypeDescriptorID of the deleted EdFiEntryTypeDescriptor.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEdFiEntryTypeDescriptor <- function(EdFiEntryTypeDescriptorID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "EdFi", objectName = "EdFiEntryTypeDescriptor", objectId = EdFiEntryTypeDescriptorID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EdFiEntryTypeDescriptor
	#'
	#' This function creates an EdFiEntryTypeDescriptor
	#' @param fieldNames The field values to give the created EdFiEntryTypeDescriptor. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return A newly created EdFiEntryTypeDescriptor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEdFiEntryTypeDescriptor <- function(Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "EdFi", objectName = "EdFiEntryTypeDescriptor", body = list(DataObject = body), searchFields = append("EdFiEntryTypeDescriptorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EdFiEntryTypeDescriptor
	#'
	#' This function modifies an EdFiEntryTypeDescriptor
	#' @param fieldNames The field values to give the modified EdFiEntryTypeDescriptor. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return The modified EdFiEntryTypeDescriptor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEdFiEntryTypeDescriptor <- function(EdFiEntryTypeDescriptorID, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "EdFi", objectName = "EdFiEntryTypeDescriptor", objectId = EdFiEntryTypeDescriptorID, body = list(DataObject = body), searchFields = append("EdFiEntryTypeDescriptorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EdFiExitWithdrawTypeDescriptors
	#'
	#' This function returns a dataframe or json object of EdFiExitWithdrawTypeDescriptors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EdFiExitWithdrawTypeDescriptors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EdFiExitWithdrawTypeDescriptors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EdFiExitWithdrawTypeDescriptor') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return A list of EdFiExitWithdrawTypeDescriptors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEdFiExitWithdrawTypeDescriptors <- function(searchConditionsList = NULL, EdFiExitWithdrawTypeDescriptorID = F, Code = F, Description = F, CodeDescription = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "EdFi", objectName = "EdFiExitWithdrawTypeDescriptor", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EdFiExitWithdrawTypeDescriptor
	#'
	#' This function returns a dataframe or json object of an EdFiExitWithdrawTypeDescriptor
	#' @param EdFiExitWithdrawTypeDescriptorID The ID of the EdFiExitWithdrawTypeDescriptor to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EdFiExitWithdrawTypeDescriptor. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EdFiExitWithdrawTypeDescriptor.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EdFiExitWithdrawTypeDescriptor') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return A dataframe or of EdFiExitWithdrawTypeDescriptor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEdFiExitWithdrawTypeDescriptor <- function(EdFiExitWithdrawTypeDescriptorID, Code = F, Description = F, CodeDescription = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EdFiExitWithdrawTypeDescriptorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "EdFi", objectName = "EdFiExitWithdrawTypeDescriptor", objectId = EdFiExitWithdrawTypeDescriptorID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EdFiExitWithdrawTypeDescriptor
	#'
	#' This function deletes an EdFiExitWithdrawTypeDescriptor
	#' @param EdFiExitWithdrawTypeDescriptorID The ID of the EdFiExitWithdrawTypeDescriptor to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return The EdFiExitWithdrawTypeDescriptorID of the deleted EdFiExitWithdrawTypeDescriptor.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEdFiExitWithdrawTypeDescriptor <- function(EdFiExitWithdrawTypeDescriptorID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "EdFi", objectName = "EdFiExitWithdrawTypeDescriptor", objectId = EdFiExitWithdrawTypeDescriptorID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EdFiExitWithdrawTypeDescriptor
	#'
	#' This function creates an EdFiExitWithdrawTypeDescriptor
	#' @param fieldNames The field values to give the created EdFiExitWithdrawTypeDescriptor. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return A newly created EdFiExitWithdrawTypeDescriptor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEdFiExitWithdrawTypeDescriptor <- function(Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "EdFi", objectName = "EdFiExitWithdrawTypeDescriptor", body = list(DataObject = body), searchFields = append("EdFiExitWithdrawTypeDescriptorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EdFiExitWithdrawTypeDescriptor
	#'
	#' This function modifies an EdFiExitWithdrawTypeDescriptor
	#' @param fieldNames The field values to give the modified EdFiExitWithdrawTypeDescriptor. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return The modified EdFiExitWithdrawTypeDescriptor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEdFiExitWithdrawTypeDescriptor <- function(EdFiExitWithdrawTypeDescriptorID, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "EdFi", objectName = "EdFiExitWithdrawTypeDescriptor", objectId = EdFiExitWithdrawTypeDescriptorID, body = list(DataObject = body), searchFields = append("EdFiExitWithdrawTypeDescriptorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EdFiLanguageDescriptors
	#'
	#' This function returns a dataframe or json object of EdFiLanguageDescriptors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EdFiLanguageDescriptors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EdFiLanguageDescriptors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EdFiLanguageDescriptor') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return A list of EdFiLanguageDescriptors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEdFiLanguageDescriptors <- function(searchConditionsList = NULL, EdFiLanguageDescriptorID = F, Code = F, Description = F, CodeDescription = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "EdFi", objectName = "EdFiLanguageDescriptor", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EdFiLanguageDescriptor
	#'
	#' This function returns a dataframe or json object of an EdFiLanguageDescriptor
	#' @param EdFiLanguageDescriptorID The ID of the EdFiLanguageDescriptor to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EdFiLanguageDescriptor. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EdFiLanguageDescriptor.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EdFiLanguageDescriptor') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return A dataframe or of EdFiLanguageDescriptor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEdFiLanguageDescriptor <- function(EdFiLanguageDescriptorID, Code = F, Description = F, CodeDescription = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EdFiLanguageDescriptorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "EdFi", objectName = "EdFiLanguageDescriptor", objectId = EdFiLanguageDescriptorID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EdFiLanguageDescriptor
	#'
	#' This function deletes an EdFiLanguageDescriptor
	#' @param EdFiLanguageDescriptorID The ID of the EdFiLanguageDescriptor to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return The EdFiLanguageDescriptorID of the deleted EdFiLanguageDescriptor.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEdFiLanguageDescriptor <- function(EdFiLanguageDescriptorID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "EdFi", objectName = "EdFiLanguageDescriptor", objectId = EdFiLanguageDescriptorID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EdFiLanguageDescriptor
	#'
	#' This function creates an EdFiLanguageDescriptor
	#' @param fieldNames The field values to give the created EdFiLanguageDescriptor. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return A newly created EdFiLanguageDescriptor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEdFiLanguageDescriptor <- function(Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "EdFi", objectName = "EdFiLanguageDescriptor", body = list(DataObject = body), searchFields = append("EdFiLanguageDescriptorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EdFiLanguageDescriptor
	#'
	#' This function modifies an EdFiLanguageDescriptor
	#' @param fieldNames The field values to give the modified EdFiLanguageDescriptor. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return The modified EdFiLanguageDescriptor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEdFiLanguageDescriptor <- function(EdFiLanguageDescriptorID, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "EdFi", objectName = "EdFiLanguageDescriptor", objectId = EdFiLanguageDescriptorID, body = list(DataObject = body), searchFields = append("EdFiLanguageDescriptorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EdFiSchoolFoodServiceProgramServiceTypes
	#'
	#' This function returns a dataframe or json object of EdFiSchoolFoodServiceProgramServiceTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EdFiSchoolFoodServiceProgramServiceTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EdFiSchoolFoodServiceProgramServiceTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EdFiSchoolFoodServiceProgramServiceType') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return A list of EdFiSchoolFoodServiceProgramServiceTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEdFiSchoolFoodServiceProgramServiceTypes <- function(searchConditionsList = NULL, EdFiSchoolFoodServiceProgramServiceTypeID = F, Code = F, Description = F, CodeDescription = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "EdFi", objectName = "EdFiSchoolFoodServiceProgramServiceType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EdFiSchoolFoodServiceProgramServiceType
	#'
	#' This function returns a dataframe or json object of an EdFiSchoolFoodServiceProgramServiceType
	#' @param EdFiSchoolFoodServiceProgramServiceTypeID The ID of the EdFiSchoolFoodServiceProgramServiceType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EdFiSchoolFoodServiceProgramServiceType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EdFiSchoolFoodServiceProgramServiceType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EdFiSchoolFoodServiceProgramServiceType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return A dataframe or of EdFiSchoolFoodServiceProgramServiceType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEdFiSchoolFoodServiceProgramServiceType <- function(EdFiSchoolFoodServiceProgramServiceTypeID, Code = F, Description = F, CodeDescription = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EdFiSchoolFoodServiceProgramServiceTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "EdFi", objectName = "EdFiSchoolFoodServiceProgramServiceType", objectId = EdFiSchoolFoodServiceProgramServiceTypeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EdFiSchoolFoodServiceProgramServiceType
	#'
	#' This function deletes an EdFiSchoolFoodServiceProgramServiceType
	#' @param EdFiSchoolFoodServiceProgramServiceTypeID The ID of the EdFiSchoolFoodServiceProgramServiceType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return The EdFiSchoolFoodServiceProgramServiceTypeID of the deleted EdFiSchoolFoodServiceProgramServiceType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEdFiSchoolFoodServiceProgramServiceType <- function(EdFiSchoolFoodServiceProgramServiceTypeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "EdFi", objectName = "EdFiSchoolFoodServiceProgramServiceType", objectId = EdFiSchoolFoodServiceProgramServiceTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EdFiSchoolFoodServiceProgramServiceType
	#'
	#' This function creates an EdFiSchoolFoodServiceProgramServiceType
	#' @param fieldNames The field values to give the created EdFiSchoolFoodServiceProgramServiceType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return A newly created EdFiSchoolFoodServiceProgramServiceType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEdFiSchoolFoodServiceProgramServiceType <- function(Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "EdFi", objectName = "EdFiSchoolFoodServiceProgramServiceType", body = list(DataObject = body), searchFields = append("EdFiSchoolFoodServiceProgramServiceTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EdFiSchoolFoodServiceProgramServiceType
	#'
	#' This function modifies an EdFiSchoolFoodServiceProgramServiceType
	#' @param fieldNames The field values to give the modified EdFiSchoolFoodServiceProgramServiceType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return The modified EdFiSchoolFoodServiceProgramServiceType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEdFiSchoolFoodServiceProgramServiceType <- function(EdFiSchoolFoodServiceProgramServiceTypeID, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "EdFi", objectName = "EdFiSchoolFoodServiceProgramServiceType", objectId = EdFiSchoolFoodServiceProgramServiceTypeID, body = list(DataObject = body), searchFields = append("EdFiSchoolFoodServiceProgramServiceTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EdFiRelationDescriptors
	#'
	#' This function returns a dataframe or json object of EdFiRelationDescriptors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EdFiRelationDescriptors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EdFiRelationDescriptors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EdFiRelationDescriptor') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return A list of EdFiRelationDescriptors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEdFiRelationDescriptors <- function(searchConditionsList = NULL, EdFiRelationDescriptorID = F, Code = F, Description = F, CodeDescription = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "EdFi", objectName = "EdFiRelationDescriptor", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EdFiRelationDescriptor
	#'
	#' This function returns a dataframe or json object of an EdFiRelationDescriptor
	#' @param EdFiRelationDescriptorID The ID of the EdFiRelationDescriptor to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EdFiRelationDescriptor. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EdFiRelationDescriptor.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EdFiRelationDescriptor') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return A dataframe or of EdFiRelationDescriptor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEdFiRelationDescriptor <- function(EdFiRelationDescriptorID, Code = F, Description = F, CodeDescription = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EdFiRelationDescriptorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "EdFi", objectName = "EdFiRelationDescriptor", objectId = EdFiRelationDescriptorID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EdFiRelationDescriptor
	#'
	#' This function deletes an EdFiRelationDescriptor
	#' @param EdFiRelationDescriptorID The ID of the EdFiRelationDescriptor to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return The EdFiRelationDescriptorID of the deleted EdFiRelationDescriptor.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEdFiRelationDescriptor <- function(EdFiRelationDescriptorID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "EdFi", objectName = "EdFiRelationDescriptor", objectId = EdFiRelationDescriptorID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EdFiRelationDescriptor
	#'
	#' This function creates an EdFiRelationDescriptor
	#' @param fieldNames The field values to give the created EdFiRelationDescriptor. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return A newly created EdFiRelationDescriptor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEdFiRelationDescriptor <- function(Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "EdFi", objectName = "EdFiRelationDescriptor", body = list(DataObject = body), searchFields = append("EdFiRelationDescriptorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EdFiRelationDescriptor
	#'
	#' This function modifies an EdFiRelationDescriptor
	#' @param fieldNames The field values to give the modified EdFiRelationDescriptor. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return The modified EdFiRelationDescriptor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEdFiRelationDescriptor <- function(EdFiRelationDescriptorID, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "EdFi", objectName = "EdFiRelationDescriptor", objectId = EdFiRelationDescriptorID, body = list(DataObject = body), searchFields = append("EdFiRelationDescriptorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EdFiTelephoneNumberTypeDescriptors
	#'
	#' This function returns a dataframe or json object of EdFiTelephoneNumberTypeDescriptors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EdFiTelephoneNumberTypeDescriptors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EdFiTelephoneNumberTypeDescriptors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EdFiTelephoneNumberTypeDescriptor') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return A list of EdFiTelephoneNumberTypeDescriptors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEdFiTelephoneNumberTypeDescriptors <- function(searchConditionsList = NULL, EdFiTelephoneNumberTypeDescriptorID = F, Code = F, Description = F, CodeDescription = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "EdFi", objectName = "EdFiTelephoneNumberTypeDescriptor", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EdFiTelephoneNumberTypeDescriptor
	#'
	#' This function returns a dataframe or json object of an EdFiTelephoneNumberTypeDescriptor
	#' @param EdFiTelephoneNumberTypeDescriptorID The ID of the EdFiTelephoneNumberTypeDescriptor to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EdFiTelephoneNumberTypeDescriptor. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EdFiTelephoneNumberTypeDescriptor.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EdFiTelephoneNumberTypeDescriptor') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return A dataframe or of EdFiTelephoneNumberTypeDescriptor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEdFiTelephoneNumberTypeDescriptor <- function(EdFiTelephoneNumberTypeDescriptorID, Code = F, Description = F, CodeDescription = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EdFiTelephoneNumberTypeDescriptorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "EdFi", objectName = "EdFiTelephoneNumberTypeDescriptor", objectId = EdFiTelephoneNumberTypeDescriptorID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EdFiTelephoneNumberTypeDescriptor
	#'
	#' This function deletes an EdFiTelephoneNumberTypeDescriptor
	#' @param EdFiTelephoneNumberTypeDescriptorID The ID of the EdFiTelephoneNumberTypeDescriptor to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return The EdFiTelephoneNumberTypeDescriptorID of the deleted EdFiTelephoneNumberTypeDescriptor.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEdFiTelephoneNumberTypeDescriptor <- function(EdFiTelephoneNumberTypeDescriptorID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "EdFi", objectName = "EdFiTelephoneNumberTypeDescriptor", objectId = EdFiTelephoneNumberTypeDescriptorID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EdFiTelephoneNumberTypeDescriptor
	#'
	#' This function creates an EdFiTelephoneNumberTypeDescriptor
	#' @param fieldNames The field values to give the created EdFiTelephoneNumberTypeDescriptor. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return A newly created EdFiTelephoneNumberTypeDescriptor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEdFiTelephoneNumberTypeDescriptor <- function(Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "EdFi", objectName = "EdFiTelephoneNumberTypeDescriptor", body = list(DataObject = body), searchFields = append("EdFiTelephoneNumberTypeDescriptorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EdFiTelephoneNumberTypeDescriptor
	#'
	#' This function modifies an EdFiTelephoneNumberTypeDescriptor
	#' @param fieldNames The field values to give the modified EdFiTelephoneNumberTypeDescriptor. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return The modified EdFiTelephoneNumberTypeDescriptor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEdFiTelephoneNumberTypeDescriptor <- function(EdFiTelephoneNumberTypeDescriptorID, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "EdFi", objectName = "EdFiTelephoneNumberTypeDescriptor", objectId = EdFiTelephoneNumberTypeDescriptorID, body = list(DataObject = body), searchFields = append("EdFiTelephoneNumberTypeDescriptorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EdFiEdFiLevelOfEducationTypes
	#'
	#' This function returns a dataframe or json object of EdFiEdFiLevelOfEducationTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EdFiEdFiLevelOfEducationTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EdFiEdFiLevelOfEducationTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EdFiEdFiLevelOfEducationType') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return A list of EdFiEdFiLevelOfEducationTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEdFiEdFiLevelOfEducationTypes <- function(searchConditionsList = NULL, EdFiLevelOfEducationTypeID = F, Code = F, Description = F, CodeDescription = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "EdFi", objectName = "EdFiLevelOfEducationType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EdFiEdFiLevelOfEducationType
	#'
	#' This function returns a dataframe or json object of an EdFiEdFiLevelOfEducationType
	#' @param EdFiEdFiLevelOfEducationTypeID The ID of the EdFiEdFiLevelOfEducationType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EdFiEdFiLevelOfEducationType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EdFiEdFiLevelOfEducationType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EdFiEdFiLevelOfEducationType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return A dataframe or of EdFiEdFiLevelOfEducationType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEdFiEdFiLevelOfEducationType <- function(EdFiEdFiLevelOfEducationTypeID, EdFiLevelOfEducationTypeID = F, Code = F, Description = F, CodeDescription = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EdFiEdFiLevelOfEducationTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "EdFi", objectName = "EdFiLevelOfEducationType", objectId = EdFiEdFiLevelOfEducationTypeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EdFiEdFiLevelOfEducationType
	#'
	#' This function deletes an EdFiEdFiLevelOfEducationType
	#' @param EdFiEdFiLevelOfEducationTypeID The ID of the EdFiEdFiLevelOfEducationType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return The EdFiEdFiLevelOfEducationTypeID of the deleted EdFiEdFiLevelOfEducationType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEdFiEdFiLevelOfEducationType <- function(EdFiEdFiLevelOfEducationTypeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "EdFi", objectName = "EdFiLevelOfEducationType", objectId = EdFiEdFiLevelOfEducationTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EdFiEdFiLevelOfEducationType
	#'
	#' This function creates an EdFiEdFiLevelOfEducationType
	#' @param fieldNames The field values to give the created EdFiEdFiLevelOfEducationType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return A newly created EdFiEdFiLevelOfEducationType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEdFiEdFiLevelOfEducationType <- function(Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "EdFi", objectName = "EdFiLevelOfEducationType", body = list(DataObject = body), searchFields = append("EdFiLevelOfEducationTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EdFiEdFiLevelOfEducationType
	#'
	#' This function modifies an EdFiEdFiLevelOfEducationType
	#' @param fieldNames The field values to give the modified EdFiEdFiLevelOfEducationType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Ed Fi
	#' @return The modified EdFiEdFiLevelOfEducationType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEdFiEdFiLevelOfEducationType <- function(EdFiLevelOfEducationTypeID, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "EdFi", objectName = "EdFiLevelOfEducationType", objectId = EdFiLevelOfEducationTypeID, body = list(DataObject = body), searchFields = append("EdFiLevelOfEducationTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}
