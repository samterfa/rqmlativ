
	#' List TempMassApproveErrors
	#'
	#' This function returns a dataframe or json object of TempMassApproveErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempMassApproveErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempMassApproveErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempMassApproveError') to get more field paths.
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
	#' @concept Workflow
	#' @return A list of TempMassApproveErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempMassApproveErrors <- function(searchConditionsList = NULL, ApprovalObjectDescription = F, CreatedTime = F, DataObjectID = F, ErrorMessage = F, ModifiedTime = F, TempMassApproveErrorID = F, Type = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "TempMassApproveError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempMassApproveError
	#'
	#' This function returns a dataframe or json object of a TempMassApproveError
	#' @param TempMassApproveErrorID The ID of the TempMassApproveError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempMassApproveError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempMassApproveError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempMassApproveError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A dataframe or of TempMassApproveError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempMassApproveError <- function(TempMassApproveErrorID, ApprovalObjectDescription = F, CreatedTime = F, DataObjectID = F, ErrorMessage = F, ModifiedTime = F, Type = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempMassApproveErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Workflow", objectName = "TempMassApproveError", objectId = TempMassApproveErrorID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempMassApproveError
	#'
	#' This function deletes a TempMassApproveError
	#' @param TempMassApproveErrorID The ID of the TempMassApproveError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The TempMassApproveErrorID of the deleted TempMassApproveError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempMassApproveError <- function(TempMassApproveErrorID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Workflow", objectName = "TempMassApproveError", objectId = TempMassApproveErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempMassApproveError
	#'
	#' This function creates a TempMassApproveError
	#' @param fieldNames The field values to give the created TempMassApproveError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A newly created TempMassApproveError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempMassApproveError <- function(ApprovalObjectDescription = NULL, DataObjectID = NULL, ErrorMessage = NULL, Type = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Workflow", objectName = "TempMassApproveError", body = list(DataObject = body), searchFields = append("TempMassApproveErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempMassApproveError
	#'
	#' This function modifies a TempMassApproveError
	#' @param fieldNames The field values to give the modified TempMassApproveError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The modified TempMassApproveError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempMassApproveError <- function(TempMassApproveErrorID, ApprovalObjectDescription = NULL, DataObjectID = NULL, ErrorMessage = NULL, Type = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Workflow", objectName = "TempMassApproveError", objectId = TempMassApproveErrorID, body = list(DataObject = body), searchFields = append("TempMassApproveErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List WorkflowParameterMappings
	#'
	#' This function returns a dataframe or json object of WorkflowParameterMappings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given WorkflowParameterMappings. Defaults to FALSE for all return fields which, for convenience, returns all fields for the WorkflowParameterMappings.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('WorkflowParameterMapping') to get more field paths.
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
	#' @concept Workflow
	#' @return A list of WorkflowParameterMappings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listWorkflowParameterMappings <- function(searchConditionsList = NULL, CreatedTime = F, DataSource = F, ModifiedTime = F, ObjectWorkflowTriggerID = F, ParameterName = F, UserIDCreatedBy = F, UserIDModifiedBy = F, WorkflowParameterMappingID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "WorkflowParameterMapping", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a WorkflowParameterMapping
	#'
	#' This function returns a dataframe or json object of a WorkflowParameterMapping
	#' @param WorkflowParameterMappingID The ID of the WorkflowParameterMapping to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given WorkflowParameterMapping. Defaults to FALSE for all return fields which, for convenience, returns all fields for the WorkflowParameterMapping.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('WorkflowParameterMapping') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A dataframe or of WorkflowParameterMapping
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getWorkflowParameterMapping <- function(WorkflowParameterMappingID, CreatedTime = F, DataSource = F, ModifiedTime = F, ObjectWorkflowTriggerID = F, ParameterName = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "WorkflowParameterMappingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Workflow", objectName = "WorkflowParameterMapping", objectId = WorkflowParameterMappingID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a WorkflowParameterMapping
	#'
	#' This function deletes a WorkflowParameterMapping
	#' @param WorkflowParameterMappingID The ID of the WorkflowParameterMapping to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The WorkflowParameterMappingID of the deleted WorkflowParameterMapping.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteWorkflowParameterMapping <- function(WorkflowParameterMappingID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Workflow", objectName = "WorkflowParameterMapping", objectId = WorkflowParameterMappingID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a WorkflowParameterMapping
	#'
	#' This function creates a WorkflowParameterMapping
	#' @param fieldNames The field values to give the created WorkflowParameterMapping. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A newly created WorkflowParameterMapping
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createWorkflowParameterMapping <- function(DataSource = NULL, ObjectWorkflowTriggerID = NULL, ParameterName = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Workflow", objectName = "WorkflowParameterMapping", body = list(DataObject = body), searchFields = append("WorkflowParameterMappingID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a WorkflowParameterMapping
	#'
	#' This function modifies a WorkflowParameterMapping
	#' @param fieldNames The field values to give the modified WorkflowParameterMapping. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The modified WorkflowParameterMapping
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyWorkflowParameterMapping <- function(WorkflowParameterMappingID, DataSource = NULL, ObjectWorkflowTriggerID = NULL, ParameterName = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Workflow", objectName = "WorkflowParameterMapping", objectId = WorkflowParameterMappingID, body = list(DataObject = body), searchFields = append("WorkflowParameterMappingID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ObjectWorkflowTriggerSecurityGroups
	#'
	#' This function returns a dataframe or json object of ObjectWorkflowTriggerSecurityGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ObjectWorkflowTriggerSecurityGroups. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ObjectWorkflowTriggerSecurityGroups.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ObjectWorkflowTriggerSecurityGroup') to get more field paths.
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
	#' @concept Workflow
	#' @return A list of ObjectWorkflowTriggerSecurityGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listObjectWorkflowTriggerSecurityGroups <- function(searchConditionsList = NULL, CreatedTime = F, ModifiedTime = F, ObjectWorkflowTriggerID = F, ObjectWorkflowTriggerSecurityGroupID = F, SecurityGroupIDAssigned = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "ObjectWorkflowTriggerSecurityGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ObjectWorkflowTriggerSecurityGroup
	#'
	#' This function returns a dataframe or json object of an ObjectWorkflowTriggerSecurityGroup
	#' @param ObjectWorkflowTriggerSecurityGroupID The ID of the ObjectWorkflowTriggerSecurityGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ObjectWorkflowTriggerSecurityGroup. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ObjectWorkflowTriggerSecurityGroup.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ObjectWorkflowTriggerSecurityGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A dataframe or of ObjectWorkflowTriggerSecurityGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getObjectWorkflowTriggerSecurityGroup <- function(ObjectWorkflowTriggerSecurityGroupID, CreatedTime = F, ModifiedTime = F, ObjectWorkflowTriggerID = F, SecurityGroupIDAssigned = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ObjectWorkflowTriggerSecurityGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Workflow", objectName = "ObjectWorkflowTriggerSecurityGroup", objectId = ObjectWorkflowTriggerSecurityGroupID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ObjectWorkflowTriggerSecurityGroup
	#'
	#' This function deletes an ObjectWorkflowTriggerSecurityGroup
	#' @param ObjectWorkflowTriggerSecurityGroupID The ID of the ObjectWorkflowTriggerSecurityGroup to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The ObjectWorkflowTriggerSecurityGroupID of the deleted ObjectWorkflowTriggerSecurityGroup.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteObjectWorkflowTriggerSecurityGroup <- function(ObjectWorkflowTriggerSecurityGroupID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Workflow", objectName = "ObjectWorkflowTriggerSecurityGroup", objectId = ObjectWorkflowTriggerSecurityGroupID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ObjectWorkflowTriggerSecurityGroup
	#'
	#' This function creates an ObjectWorkflowTriggerSecurityGroup
	#' @param fieldNames The field values to give the created ObjectWorkflowTriggerSecurityGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A newly created ObjectWorkflowTriggerSecurityGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createObjectWorkflowTriggerSecurityGroup <- function(ObjectWorkflowTriggerID = NULL, SecurityGroupIDAssigned = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Workflow", objectName = "ObjectWorkflowTriggerSecurityGroup", body = list(DataObject = body), searchFields = append("ObjectWorkflowTriggerSecurityGroupID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ObjectWorkflowTriggerSecurityGroup
	#'
	#' This function modifies an ObjectWorkflowTriggerSecurityGroup
	#' @param fieldNames The field values to give the modified ObjectWorkflowTriggerSecurityGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The modified ObjectWorkflowTriggerSecurityGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyObjectWorkflowTriggerSecurityGroup <- function(ObjectWorkflowTriggerSecurityGroupID, ObjectWorkflowTriggerID = NULL, SecurityGroupIDAssigned = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Workflow", objectName = "ObjectWorkflowTriggerSecurityGroup", objectId = ObjectWorkflowTriggerSecurityGroupID, body = list(DataObject = body), searchFields = append("ObjectWorkflowTriggerSecurityGroupID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CustomTasks
	#'
	#' This function returns a dataframe or json object of CustomTasks
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CustomTasks. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CustomTasks.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CustomTask') to get more field paths.
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
	#' @concept Workflow
	#' @return A list of CustomTasks
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCustomTasks <- function(searchConditionsList = NULL, CreatedTime = F, CustomTaskID = F, CustomWorkflowID = F, DataXML = F, Description = F, ModifiedTime = F, Name = F, NextButtonText = F, Order = F, TaskType = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "CustomTask", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CustomTask
	#'
	#' This function returns a dataframe or json object of a CustomTask
	#' @param CustomTaskID The ID of the CustomTask to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CustomTask. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CustomTask.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CustomTask') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A dataframe or of CustomTask
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCustomTask <- function(CustomTaskID, CreatedTime = F, CustomWorkflowID = F, DataXML = F, Description = F, ModifiedTime = F, Name = F, NextButtonText = F, Order = F, TaskType = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CustomTaskID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Workflow", objectName = "CustomTask", objectId = CustomTaskID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CustomTask
	#'
	#' This function deletes a CustomTask
	#' @param CustomTaskID The ID of the CustomTask to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The CustomTaskID of the deleted CustomTask.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCustomTask <- function(CustomTaskID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Workflow", objectName = "CustomTask", objectId = CustomTaskID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CustomTask
	#'
	#' This function creates a CustomTask
	#' @param fieldNames The field values to give the created CustomTask. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A newly created CustomTask
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCustomTask <- function(CustomWorkflowID = NULL, DataXML = NULL, Description = NULL, Name = NULL, NextButtonText = NULL, Order = NULL, TaskType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Workflow", objectName = "CustomTask", body = list(DataObject = body), searchFields = append("CustomTaskID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CustomTask
	#'
	#' This function modifies a CustomTask
	#' @param fieldNames The field values to give the modified CustomTask. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The modified CustomTask
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCustomTask <- function(CustomTaskID, CustomWorkflowID = NULL, DataXML = NULL, Description = NULL, Name = NULL, NextButtonText = NULL, Order = NULL, TaskType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Workflow", objectName = "CustomTask", objectId = CustomTaskID, body = list(DataObject = body), searchFields = append("CustomTaskID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CustomTaskAssignedTos
	#'
	#' This function returns a dataframe or json object of CustomTaskAssignedTos
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CustomTaskAssignedTos. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CustomTaskAssignedTos.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CustomTaskAssignedTo') to get more field paths.
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
	#' @concept Workflow
	#' @return A list of CustomTaskAssignedTos
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCustomTaskAssignedTos <- function(searchConditionsList = NULL, CreatedTime = F, CustomTaskAssignedToID = F, CustomTaskID = F, FieldIDToApplyUserTaskAccess = F, GroupIDSecurity = F, ModifiedTime = F, UserIDAssignedTo = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "CustomTaskAssignedTo", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CustomTaskAssignedTo
	#'
	#' This function returns a dataframe or json object of a CustomTaskAssignedTo
	#' @param CustomTaskAssignedToID The ID of the CustomTaskAssignedTo to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CustomTaskAssignedTo. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CustomTaskAssignedTo.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CustomTaskAssignedTo') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A dataframe or of CustomTaskAssignedTo
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCustomTaskAssignedTo <- function(CustomTaskAssignedToID, CreatedTime = F, CustomTaskID = F, FieldIDToApplyUserTaskAccess = F, GroupIDSecurity = F, ModifiedTime = F, UserIDAssignedTo = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CustomTaskAssignedToID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Workflow", objectName = "CustomTaskAssignedTo", objectId = CustomTaskAssignedToID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CustomTaskAssignedTo
	#'
	#' This function deletes a CustomTaskAssignedTo
	#' @param CustomTaskAssignedToID The ID of the CustomTaskAssignedTo to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The CustomTaskAssignedToID of the deleted CustomTaskAssignedTo.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCustomTaskAssignedTo <- function(CustomTaskAssignedToID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Workflow", objectName = "CustomTaskAssignedTo", objectId = CustomTaskAssignedToID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CustomTaskAssignedTo
	#'
	#' This function creates a CustomTaskAssignedTo
	#' @param fieldNames The field values to give the created CustomTaskAssignedTo. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A newly created CustomTaskAssignedTo
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCustomTaskAssignedTo <- function(CustomTaskID = NULL, FieldIDToApplyUserTaskAccess = NULL, GroupIDSecurity = NULL, UserIDAssignedTo = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Workflow", objectName = "CustomTaskAssignedTo", body = list(DataObject = body), searchFields = append("CustomTaskAssignedToID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CustomTaskAssignedTo
	#'
	#' This function modifies a CustomTaskAssignedTo
	#' @param fieldNames The field values to give the modified CustomTaskAssignedTo. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The modified CustomTaskAssignedTo
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCustomTaskAssignedTo <- function(CustomTaskAssignedToID, CustomTaskID = NULL, FieldIDToApplyUserTaskAccess = NULL, GroupIDSecurity = NULL, UserIDAssignedTo = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Workflow", objectName = "CustomTaskAssignedTo", objectId = CustomTaskAssignedToID, body = list(DataObject = body), searchFields = append("CustomTaskAssignedToID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CustomUITaskElements
	#'
	#' This function returns a dataframe or json object of CustomUITaskElements
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CustomUITaskElements. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CustomUITaskElements.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CustomUITaskElement') to get more field paths.
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
	#' @concept Workflow
	#' @return A list of CustomUITaskElements
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCustomUITaskElements <- function(searchConditionsList = NULL, CreatedTime = F, CustomTaskID = F, CustomUITaskElementID = F, ElementType = F, FieldID = F, LimitTextField = F, LinkHref = F, LinkTextValue = F, ModifiedTime = F, Order = F, TextFieldOptions = F, UITextValue = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "CustomUITaskElement", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CustomUITaskElement
	#'
	#' This function returns a dataframe or json object of a CustomUITaskElement
	#' @param CustomUITaskElementID The ID of the CustomUITaskElement to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CustomUITaskElement. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CustomUITaskElement.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CustomUITaskElement') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A dataframe or of CustomUITaskElement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCustomUITaskElement <- function(CustomUITaskElementID, CreatedTime = F, CustomTaskID = F, ElementType = F, FieldID = F, LimitTextField = F, LinkHref = F, LinkTextValue = F, ModifiedTime = F, Order = F, TextFieldOptions = F, UITextValue = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CustomUITaskElementID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Workflow", objectName = "CustomUITaskElement", objectId = CustomUITaskElementID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CustomUITaskElement
	#'
	#' This function deletes a CustomUITaskElement
	#' @param CustomUITaskElementID The ID of the CustomUITaskElement to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The CustomUITaskElementID of the deleted CustomUITaskElement.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCustomUITaskElement <- function(CustomUITaskElementID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Workflow", objectName = "CustomUITaskElement", objectId = CustomUITaskElementID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CustomUITaskElement
	#'
	#' This function creates a CustomUITaskElement
	#' @param fieldNames The field values to give the created CustomUITaskElement. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A newly created CustomUITaskElement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCustomUITaskElement <- function(CustomTaskID = NULL, ElementType = NULL, FieldID = NULL, LimitTextField = NULL, LinkHref = NULL, LinkTextValue = NULL, Order = NULL, TextFieldOptions = NULL, UITextValue = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Workflow", objectName = "CustomUITaskElement", body = list(DataObject = body), searchFields = append("CustomUITaskElementID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CustomUITaskElement
	#'
	#' This function modifies a CustomUITaskElement
	#' @param fieldNames The field values to give the modified CustomUITaskElement. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The modified CustomUITaskElement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCustomUITaskElement <- function(CustomUITaskElementID, CustomTaskID = NULL, ElementType = NULL, FieldID = NULL, LimitTextField = NULL, LinkHref = NULL, LinkTextValue = NULL, Order = NULL, TextFieldOptions = NULL, UITextValue = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Workflow", objectName = "CustomUITaskElement", objectId = CustomUITaskElementID, body = list(DataObject = body), searchFields = append("CustomUITaskElementID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CustomWorkflows
	#'
	#' This function returns a dataframe or json object of CustomWorkflows
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CustomWorkflows. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CustomWorkflows.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CustomWorkflow') to get more field paths.
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
	#' @concept Workflow
	#' @return A list of CustomWorkflows
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCustomWorkflows <- function(searchConditionsList = NULL, CreatedTime = F, CustomWorkflowID = F, Description = F, ModifiedTime = F, Module = F, Name = F, Object = F, ObjectIDRoot = F, ScreenAttribute = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "CustomWorkflow", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CustomWorkflow
	#'
	#' This function returns a dataframe or json object of a CustomWorkflow
	#' @param CustomWorkflowID The ID of the CustomWorkflow to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CustomWorkflow. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CustomWorkflow.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CustomWorkflow') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A dataframe or of CustomWorkflow
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCustomWorkflow <- function(CustomWorkflowID, CreatedTime = F, Description = F, ModifiedTime = F, Module = F, Name = F, Object = F, ObjectIDRoot = F, ScreenAttribute = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CustomWorkflowID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Workflow", objectName = "CustomWorkflow", objectId = CustomWorkflowID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CustomWorkflow
	#'
	#' This function deletes a CustomWorkflow
	#' @param CustomWorkflowID The ID of the CustomWorkflow to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The CustomWorkflowID of the deleted CustomWorkflow.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCustomWorkflow <- function(CustomWorkflowID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Workflow", objectName = "CustomWorkflow", objectId = CustomWorkflowID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CustomWorkflow
	#'
	#' This function creates a CustomWorkflow
	#' @param fieldNames The field values to give the created CustomWorkflow. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A newly created CustomWorkflow
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCustomWorkflow <- function(Description = NULL, Module = NULL, Name = NULL, Object = NULL, ObjectIDRoot = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Workflow", objectName = "CustomWorkflow", body = list(DataObject = body), searchFields = append("CustomWorkflowID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CustomWorkflow
	#'
	#' This function modifies a CustomWorkflow
	#' @param fieldNames The field values to give the modified CustomWorkflow. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The modified CustomWorkflow
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCustomWorkflow <- function(CustomWorkflowID, Description = NULL, Module = NULL, Name = NULL, Object = NULL, ObjectIDRoot = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Workflow", objectName = "CustomWorkflow", objectId = CustomWorkflowID, body = list(DataObject = body), searchFields = append("CustomWorkflowID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TaskActionData
	#'
	#' This function returns a dataframe or json object of TaskActionData
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TaskActionData. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TaskActionData.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TaskActionData') to get more field paths.
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
	#' @concept Workflow
	#' @return A list of TaskActionData
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTaskActionData <- function(searchConditionsList = NULL, ClassName = F, CreatedTime = F, Data = F, ModifiedTime = F, Order = F, TaskActionDataID = F, TaskID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "TaskActionData", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TaskActionData
	#'
	#' This function returns a dataframe or json object of a TaskActionData
	#' @param TaskActionDataID The ID of the TaskActionData to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TaskActionData. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TaskActionData.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TaskActionData') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A dataframe or of TaskActionData
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTaskActionData <- function(TaskActionDataID, ClassName = F, CreatedTime = F, Data = F, ModifiedTime = F, Order = F, TaskID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TaskActionDataID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Workflow", objectName = "TaskActionData", objectId = TaskActionDataID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TaskActionData
	#'
	#' This function deletes a TaskActionData
	#' @param TaskActionDataID The ID of the TaskActionData to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The TaskActionDataID of the deleted TaskActionData.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTaskActionData <- function(TaskActionDataID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Workflow", objectName = "TaskActionData", objectId = TaskActionDataID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TaskActionData
	#'
	#' This function creates a TaskActionData
	#' @param fieldNames The field values to give the created TaskActionData. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A newly created TaskActionData
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTaskActionData <- function(ClassName = NULL, Data = NULL, Order = NULL, TaskID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Workflow", objectName = "TaskActionData", body = list(DataObject = body), searchFields = append("TaskActionDataID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TaskActionData
	#'
	#' This function modifies a TaskActionData
	#' @param fieldNames The field values to give the modified TaskActionData. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The modified TaskActionData
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTaskActionData <- function(TaskActionDataID, ClassName = NULL, Data = NULL, Order = NULL, TaskID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Workflow", objectName = "TaskActionData", objectId = TaskActionDataID, body = list(DataObject = body), searchFields = append("TaskActionDataID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ObjectWorkflowTriggers
	#'
	#' This function returns a dataframe or json object of ObjectWorkflowTriggers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ObjectWorkflowTriggers. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ObjectWorkflowTriggers.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ObjectWorkflowTrigger') to get more field paths.
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
	#' @concept Workflow
	#' @return A list of ObjectWorkflowTriggers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listObjectWorkflowTriggers <- function(searchConditionsList = NULL, CreatedTime = F, ModifiedTime = F, ObjectID = F, ObjectSaveMode = F, ObjectWorkflowTriggerID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, WorkflowID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "ObjectWorkflowTrigger", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ObjectWorkflowTrigger
	#'
	#' This function returns a dataframe or json object of an ObjectWorkflowTrigger
	#' @param ObjectWorkflowTriggerID The ID of the ObjectWorkflowTrigger to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ObjectWorkflowTrigger. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ObjectWorkflowTrigger.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ObjectWorkflowTrigger') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A dataframe or of ObjectWorkflowTrigger
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getObjectWorkflowTrigger <- function(ObjectWorkflowTriggerID, CreatedTime = F, ModifiedTime = F, ObjectID = F, ObjectSaveMode = F, UserIDCreatedBy = F, UserIDModifiedBy = F, WorkflowID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ObjectWorkflowTriggerID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Workflow", objectName = "ObjectWorkflowTrigger", objectId = ObjectWorkflowTriggerID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ObjectWorkflowTrigger
	#'
	#' This function deletes an ObjectWorkflowTrigger
	#' @param ObjectWorkflowTriggerID The ID of the ObjectWorkflowTrigger to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The ObjectWorkflowTriggerID of the deleted ObjectWorkflowTrigger.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteObjectWorkflowTrigger <- function(ObjectWorkflowTriggerID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Workflow", objectName = "ObjectWorkflowTrigger", objectId = ObjectWorkflowTriggerID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ObjectWorkflowTrigger
	#'
	#' This function creates an ObjectWorkflowTrigger
	#' @param fieldNames The field values to give the created ObjectWorkflowTrigger. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A newly created ObjectWorkflowTrigger
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createObjectWorkflowTrigger <- function(ObjectID = NULL, ObjectSaveMode = NULL, WorkflowID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Workflow", objectName = "ObjectWorkflowTrigger", body = list(DataObject = body), searchFields = append("ObjectWorkflowTriggerID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ObjectWorkflowTrigger
	#'
	#' This function modifies an ObjectWorkflowTrigger
	#' @param fieldNames The field values to give the modified ObjectWorkflowTrigger. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The modified ObjectWorkflowTrigger
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyObjectWorkflowTrigger <- function(ObjectWorkflowTriggerID, ObjectID = NULL, ObjectSaveMode = NULL, WorkflowID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Workflow", objectName = "ObjectWorkflowTrigger", objectId = ObjectWorkflowTriggerID, body = list(DataObject = body), searchFields = append("ObjectWorkflowTriggerID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TaskInstanceStatuses
	#'
	#' This function returns a dataframe or json object of TaskInstanceStatuses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TaskInstanceStatuses. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TaskInstanceStatuses.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TaskInstanceStatus') to get more field paths.
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
	#' @concept Workflow
	#' @return A list of TaskInstanceStatuses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTaskInstanceStatuses <- function(searchConditionsList = NULL, CurrentRecordIdentifier = F, CurrentRecordNumber = F, PercentComplete = F, ProcessName = F, TaskInstanceID = F, TaskInstanceStatusID = F, TotalRecordCount = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "TaskInstanceStatus", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TaskInstanceStatus
	#'
	#' This function returns a dataframe or json object of a TaskInstanceStatus
	#' @param TaskInstanceStatusID The ID of the TaskInstanceStatus to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TaskInstanceStatus. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TaskInstanceStatus.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TaskInstanceStatus') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A dataframe or of TaskInstanceStatus
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTaskInstanceStatus <- function(TaskInstanceStatusID, CurrentRecordIdentifier = F, CurrentRecordNumber = F, PercentComplete = F, ProcessName = F, TaskInstanceID = F, TotalRecordCount = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TaskInstanceStatusID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Workflow", objectName = "TaskInstanceStatus", objectId = TaskInstanceStatusID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TaskInstanceStatus
	#'
	#' This function deletes a TaskInstanceStatus
	#' @param TaskInstanceStatusID The ID of the TaskInstanceStatus to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The TaskInstanceStatusID of the deleted TaskInstanceStatus.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTaskInstanceStatus <- function(TaskInstanceStatusID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Workflow", objectName = "TaskInstanceStatus", objectId = TaskInstanceStatusID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TaskInstanceStatus
	#'
	#' This function creates a TaskInstanceStatus
	#' @param fieldNames The field values to give the created TaskInstanceStatus. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A newly created TaskInstanceStatus
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTaskInstanceStatus <- function(CurrentRecordIdentifier = NULL, CurrentRecordNumber = NULL, PercentComplete = NULL, ProcessName = NULL, TaskInstanceID = NULL, TotalRecordCount = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Workflow", objectName = "TaskInstanceStatus", body = list(DataObject = body), searchFields = append("TaskInstanceStatusID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TaskInstanceStatus
	#'
	#' This function modifies a TaskInstanceStatus
	#' @param fieldNames The field values to give the modified TaskInstanceStatus. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The modified TaskInstanceStatus
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTaskInstanceStatus <- function(TaskInstanceStatusID, CurrentRecordIdentifier = NULL, CurrentRecordNumber = NULL, PercentComplete = NULL, ProcessName = NULL, TaskInstanceID = NULL, TotalRecordCount = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Workflow", objectName = "TaskInstanceStatus", objectId = TaskInstanceStatusID, body = list(DataObject = body), searchFields = append("TaskInstanceStatusID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TaskConnections
	#'
	#' This function returns a dataframe or json object of TaskConnections
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TaskConnections. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TaskConnections.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TaskConnection') to get more field paths.
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
	#' @concept Workflow
	#' @return A list of TaskConnections
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTaskConnections <- function(searchConditionsList = NULL, Condition = F, CreatedTime = F, IsSkywardTaskConnection = F, ModifiedTime = F, TaskConnectionID = F, TaskIDNext = F, TaskIDPrevious = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "TaskConnection", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TaskConnection
	#'
	#' This function returns a dataframe or json object of a TaskConnection
	#' @param TaskConnectionID The ID of the TaskConnection to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TaskConnection. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TaskConnection.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TaskConnection') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A dataframe or of TaskConnection
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTaskConnection <- function(TaskConnectionID, Condition = F, CreatedTime = F, IsSkywardTaskConnection = F, ModifiedTime = F, TaskIDNext = F, TaskIDPrevious = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TaskConnectionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Workflow", objectName = "TaskConnection", objectId = TaskConnectionID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TaskConnection
	#'
	#' This function deletes a TaskConnection
	#' @param TaskConnectionID The ID of the TaskConnection to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The TaskConnectionID of the deleted TaskConnection.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTaskConnection <- function(TaskConnectionID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Workflow", objectName = "TaskConnection", objectId = TaskConnectionID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TaskConnection
	#'
	#' This function creates a TaskConnection
	#' @param fieldNames The field values to give the created TaskConnection. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A newly created TaskConnection
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTaskConnection <- function(IsSkywardTaskConnection = NULL, TaskIDNext = NULL, TaskIDPrevious = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Workflow", objectName = "TaskConnection", body = list(DataObject = body), searchFields = append("TaskConnectionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TaskConnection
	#'
	#' This function modifies a TaskConnection
	#' @param fieldNames The field values to give the modified TaskConnection. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The modified TaskConnection
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTaskConnection <- function(TaskConnectionID, IsSkywardTaskConnection = NULL, TaskIDNext = NULL, TaskIDPrevious = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Workflow", objectName = "TaskConnection", objectId = TaskConnectionID, body = list(DataObject = body), searchFields = append("TaskConnectionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TaskInstanceUsers
	#'
	#' This function returns a dataframe or json object of TaskInstanceUsers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TaskInstanceUsers. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TaskInstanceUsers.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TaskInstanceUser') to get more field paths.
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
	#' @concept Workflow
	#' @return A list of TaskInstanceUsers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTaskInstanceUsers <- function(searchConditionsList = NULL, CreatedTime = F, Description = F, ModifiedTime = F, TaskInstanceID = F, TaskInstanceUserID = F, Type = F, TypeCode = F, UserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "TaskInstanceUser", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TaskInstanceUser
	#'
	#' This function returns a dataframe or json object of a TaskInstanceUser
	#' @param TaskInstanceUserID The ID of the TaskInstanceUser to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TaskInstanceUser. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TaskInstanceUser.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TaskInstanceUser') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A dataframe or of TaskInstanceUser
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTaskInstanceUser <- function(TaskInstanceUserID, CreatedTime = F, Description = F, ModifiedTime = F, TaskInstanceID = F, Type = F, TypeCode = F, UserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TaskInstanceUserID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Workflow", objectName = "TaskInstanceUser", objectId = TaskInstanceUserID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TaskInstanceUser
	#'
	#' This function deletes a TaskInstanceUser
	#' @param TaskInstanceUserID The ID of the TaskInstanceUser to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The TaskInstanceUserID of the deleted TaskInstanceUser.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTaskInstanceUser <- function(TaskInstanceUserID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Workflow", objectName = "TaskInstanceUser", objectId = TaskInstanceUserID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TaskInstanceUser
	#'
	#' This function creates a TaskInstanceUser
	#' @param fieldNames The field values to give the created TaskInstanceUser. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A newly created TaskInstanceUser
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTaskInstanceUser <- function(Description = NULL, TaskInstanceID = NULL, Type = NULL, UserID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Workflow", objectName = "TaskInstanceUser", body = list(DataObject = body), searchFields = append("TaskInstanceUserID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TaskInstanceUser
	#'
	#' This function modifies a TaskInstanceUser
	#' @param fieldNames The field values to give the modified TaskInstanceUser. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The modified TaskInstanceUser
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTaskInstanceUser <- function(TaskInstanceUserID, Description = NULL, TaskInstanceID = NULL, Type = NULL, UserID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Workflow", objectName = "TaskInstanceUser", objectId = TaskInstanceUserID, body = list(DataObject = body), searchFields = append("TaskInstanceUserID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TaskInstances
	#'
	#' This function returns a dataframe or json object of TaskInstances
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TaskInstances. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TaskInstances.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TaskInstance') to get more field paths.
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
	#' @concept Workflow
	#' @return A list of TaskInstances
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTaskInstances <- function(searchConditionsList = NULL, Application = F, CreatedTime = F, Hostname = F, LastActivity = F, ModifiedTime = F, PreviousWarningHashes = F, ProcessDuration = F, ProcessID = F, Queue = F, QueueCode = F, QueuedDuration = F, QueueDescription = F, QueueTime = F, StartTime = F, Status = F, StatusCode = F, StopTime = F, TaskActionXml = F, TaskID = F, TaskInstanceID = F, ThreadName = F, UserIDCreatedBy = F, UserIDImpersonator = F, UserIDModifiedBy = F, UserIDPerformer = F, Variables = F, WorkflowInstanceID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "TaskInstance", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TaskInstance
	#'
	#' This function returns a dataframe or json object of a TaskInstance
	#' @param TaskInstanceID The ID of the TaskInstance to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TaskInstance. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TaskInstance.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TaskInstance') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A dataframe or of TaskInstance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTaskInstance <- function(TaskInstanceID, Application = F, CreatedTime = F, Hostname = F, LastActivity = F, ModifiedTime = F, PreviousWarningHashes = F, ProcessDuration = F, ProcessID = F, Queue = F, QueueCode = F, QueuedDuration = F, QueueDescription = F, QueueTime = F, StartTime = F, Status = F, StatusCode = F, StopTime = F, TaskActionXml = F, TaskID = F, ThreadName = F, UserIDCreatedBy = F, UserIDImpersonator = F, UserIDModifiedBy = F, UserIDPerformer = F, Variables = F, WorkflowInstanceID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TaskInstanceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Workflow", objectName = "TaskInstance", objectId = TaskInstanceID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TaskInstance
	#'
	#' This function deletes a TaskInstance
	#' @param TaskInstanceID The ID of the TaskInstance to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The TaskInstanceID of the deleted TaskInstance.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTaskInstance <- function(TaskInstanceID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Workflow", objectName = "TaskInstance", objectId = TaskInstanceID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TaskInstance
	#'
	#' This function creates a TaskInstance
	#' @param fieldNames The field values to give the created TaskInstance. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A newly created TaskInstance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTaskInstance <- function(Application = NULL, Hostname = NULL, LastActivity = NULL, ProcessID = NULL, Queue = NULL, QueueDescription = NULL, QueueTime = NULL, StartTime = NULL, Status = NULL, StopTime = NULL, TaskID = NULL, ThreadName = NULL, UserIDPerformer = NULL, WorkflowInstanceID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Workflow", objectName = "TaskInstance", body = list(DataObject = body), searchFields = append("TaskInstanceID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TaskInstance
	#'
	#' This function modifies a TaskInstance
	#' @param fieldNames The field values to give the modified TaskInstance. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The modified TaskInstance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTaskInstance <- function(TaskInstanceID, Application = NULL, Hostname = NULL, LastActivity = NULL, ProcessID = NULL, Queue = NULL, QueueDescription = NULL, QueueTime = NULL, StartTime = NULL, Status = NULL, StopTime = NULL, TaskID = NULL, ThreadName = NULL, UserIDPerformer = NULL, WorkflowInstanceID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Workflow", objectName = "TaskInstance", objectId = TaskInstanceID, body = list(DataObject = body), searchFields = append("TaskInstanceID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Tasks
	#'
	#' This function returns a dataframe or json object of Tasks
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Tasks. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Tasks.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Task') to get more field paths.
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
	#' @concept Workflow
	#' @return A list of Tasks
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTasks <- function(searchConditionsList = NULL, AllowCancel = F, AllowCancelCondition = F, AllowNext = F, AllowNextCondition = F, AllowPrevious = F, AllowPreviousCondition = F, CreatedTime = F, Description = F, DisableScreenValidationForUITask = F, ExpirationDurationOverrideCode = F, ExpirationDurationOverrideCustomDays = F, IsSkywardTask = F, ModifiedTime = F, Name = F, NavigationGroup = F, Queue = F, RemoveValidationContainerForUITask = F, ScreenXML = F, TaskButtonCondition = F, TaskID = F, Type = F, UserIDCreatedBy = F, UserIDModifiedBy = F, WorkflowVersionID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "Task", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Task
	#'
	#' This function returns a dataframe or json object of a Task
	#' @param TaskID The ID of the Task to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Task. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Task.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Task') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A dataframe or of Task
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTask <- function(TaskID, AllowCancel = F, AllowCancelCondition = F, AllowNext = F, AllowNextCondition = F, AllowPrevious = F, AllowPreviousCondition = F, CreatedTime = F, Description = F, DisableScreenValidationForUITask = F, ExpirationDurationOverrideCode = F, ExpirationDurationOverrideCustomDays = F, IsSkywardTask = F, ModifiedTime = F, Name = F, NavigationGroup = F, Queue = F, RemoveValidationContainerForUITask = F, ScreenXML = F, TaskButtonCondition = F, Type = F, UserIDCreatedBy = F, UserIDModifiedBy = F, WorkflowVersionID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TaskID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Workflow", objectName = "Task", objectId = TaskID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Task
	#'
	#' This function deletes a Task
	#' @param TaskID The ID of the Task to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The TaskID of the deleted Task.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTask <- function(TaskID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Workflow", objectName = "Task", objectId = TaskID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Task
	#'
	#' This function creates a Task
	#' @param fieldNames The field values to give the created Task. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A newly created Task
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTask <- function(Description = NULL, ExpirationDurationOverrideCode = NULL, ExpirationDurationOverrideCustomDays = NULL, IsSkywardTask = NULL, Name = NULL, NavigationGroup = NULL, Queue = NULL, Type = NULL, WorkflowVersionID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Workflow", objectName = "Task", body = list(DataObject = body), searchFields = append("TaskID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Task
	#'
	#' This function modifies a Task
	#' @param fieldNames The field values to give the modified Task. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The modified Task
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTask <- function(TaskID, Description = NULL, ExpirationDurationOverrideCode = NULL, ExpirationDurationOverrideCustomDays = NULL, IsSkywardTask = NULL, Name = NULL, NavigationGroup = NULL, Queue = NULL, Type = NULL, WorkflowVersionID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Workflow", objectName = "Task", objectId = TaskID, body = list(DataObject = body), searchFields = append("TaskID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Workflows
	#'
	#' This function returns a dataframe or json object of Workflows
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Workflows. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Workflows.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Workflow') to get more field paths.
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
	#' @concept Workflow
	#' @return A list of Workflows
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listWorkflows <- function(searchConditionsList = NULL, CreatedTime = F, FullName = F, HasVersions = F, IsSkywardWorkflow = F, ModifiedTime = F, Module = F, Name = F, Object = F, SearchableFullName = F, SkywardHash = F, SkywardID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, WorkflowID = F, WorkflowVersionID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "Workflow", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Workflow
	#'
	#' This function returns a dataframe or json object of a Workflow
	#' @param WorkflowID The ID of the Workflow to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Workflow. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Workflow.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Workflow') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A dataframe or of Workflow
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getWorkflow <- function(WorkflowID, CreatedTime = F, FullName = F, HasVersions = F, IsSkywardWorkflow = F, ModifiedTime = F, Module = F, Name = F, Object = F, SearchableFullName = F, SkywardHash = F, SkywardID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, WorkflowVersionID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "WorkflowID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Workflow", objectName = "Workflow", objectId = WorkflowID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Workflow
	#'
	#' This function deletes a Workflow
	#' @param WorkflowID The ID of the Workflow to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The WorkflowID of the deleted Workflow.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteWorkflow <- function(WorkflowID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Workflow", objectName = "Workflow", objectId = WorkflowID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Workflow
	#'
	#' This function creates a Workflow
	#' @param fieldNames The field values to give the created Workflow. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A newly created Workflow
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createWorkflow <- function(Module = NULL, Name = NULL, Object = NULL, WorkflowVersionID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Workflow", objectName = "Workflow", body = list(DataObject = body), searchFields = append("WorkflowID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Workflow
	#'
	#' This function modifies a Workflow
	#' @param fieldNames The field values to give the modified Workflow. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The modified Workflow
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyWorkflow <- function(WorkflowID, Module = NULL, Name = NULL, Object = NULL, WorkflowVersionID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Workflow", objectName = "Workflow", objectId = WorkflowID, body = list(DataObject = body), searchFields = append("WorkflowID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List WorkflowInstances
	#'
	#' This function returns a dataframe or json object of WorkflowInstances
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given WorkflowInstances. Defaults to FALSE for all return fields which, for convenience, returns all fields for the WorkflowInstances.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('WorkflowInstance') to get more field paths.
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
	#' @concept Workflow
	#' @return A list of WorkflowInstances
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listWorkflowInstances <- function(searchConditionsList = NULL, AbortInitiated = F, CreatedTime = F, EntityID = F, LogID = F, ModifiedTime = F, ScheduledTaskInstanceID = F, Status = F, StatusCode = F, Success = F, UserIDCreatedBy = F, UserIDImpersonator = F, UserIDModifiedBy = F, ValidationFailureBehaviorCode = F, WorkflowInstanceID = F, WorkflowInstanceIDRoot = F, WorkflowVersionID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "WorkflowInstance", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a WorkflowInstance
	#'
	#' This function returns a dataframe or json object of a WorkflowInstance
	#' @param WorkflowInstanceID The ID of the WorkflowInstance to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given WorkflowInstance. Defaults to FALSE for all return fields which, for convenience, returns all fields for the WorkflowInstance.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('WorkflowInstance') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A dataframe or of WorkflowInstance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getWorkflowInstance <- function(WorkflowInstanceID, AbortInitiated = F, CreatedTime = F, EntityID = F, LogID = F, ModifiedTime = F, ScheduledTaskInstanceID = F, Status = F, StatusCode = F, Success = F, UserIDCreatedBy = F, UserIDImpersonator = F, UserIDModifiedBy = F, ValidationFailureBehaviorCode = F, WorkflowInstanceIDRoot = F, WorkflowVersionID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "WorkflowInstanceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Workflow", objectName = "WorkflowInstance", objectId = WorkflowInstanceID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a WorkflowInstance
	#'
	#' This function deletes a WorkflowInstance
	#' @param WorkflowInstanceID The ID of the WorkflowInstance to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The WorkflowInstanceID of the deleted WorkflowInstance.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteWorkflowInstance <- function(WorkflowInstanceID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Workflow", objectName = "WorkflowInstance", objectId = WorkflowInstanceID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a WorkflowInstance
	#'
	#' This function creates a WorkflowInstance
	#' @param fieldNames The field values to give the created WorkflowInstance. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A newly created WorkflowInstance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createWorkflowInstance <- function(AbortInitiated = NULL, LogID = NULL, ScheduledTaskInstanceID = NULL, Status = NULL, Success = NULL, ValidationFailureBehaviorCode = NULL, WorkflowInstanceIDRoot = NULL, WorkflowVersionID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Workflow", objectName = "WorkflowInstance", body = list(DataObject = body), searchFields = append("WorkflowInstanceID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a WorkflowInstance
	#'
	#' This function modifies a WorkflowInstance
	#' @param fieldNames The field values to give the modified WorkflowInstance. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The modified WorkflowInstance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyWorkflowInstance <- function(WorkflowInstanceID, AbortInitiated = NULL, LogID = NULL, ScheduledTaskInstanceID = NULL, Status = NULL, Success = NULL, ValidationFailureBehaviorCode = NULL, WorkflowInstanceIDRoot = NULL, WorkflowVersionID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Workflow", objectName = "WorkflowInstance", objectId = WorkflowInstanceID, body = list(DataObject = body), searchFields = append("WorkflowInstanceID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List WorkflowInstanceVariables
	#'
	#' This function returns a dataframe or json object of WorkflowInstanceVariables
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given WorkflowInstanceVariables. Defaults to FALSE for all return fields which, for convenience, returns all fields for the WorkflowInstanceVariables.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('WorkflowInstanceVariable') to get more field paths.
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
	#' @concept Workflow
	#' @return A list of WorkflowInstanceVariables
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listWorkflowInstanceVariables <- function(searchConditionsList = NULL, ClassName = F, CreatedTime = F, IsEncrypted = F, ModifiedTime = F, Name = F, SchemaFieldUniqueID = F, SchemaObjectUniqueID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, Value = F, WorkflowInstanceID = F, WorkflowInstanceVariableID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "WorkflowInstanceVariable", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a WorkflowInstanceVariable
	#'
	#' This function returns a dataframe or json object of a WorkflowInstanceVariable
	#' @param WorkflowInstanceVariableID The ID of the WorkflowInstanceVariable to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given WorkflowInstanceVariable. Defaults to FALSE for all return fields which, for convenience, returns all fields for the WorkflowInstanceVariable.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('WorkflowInstanceVariable') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A dataframe or of WorkflowInstanceVariable
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getWorkflowInstanceVariable <- function(WorkflowInstanceVariableID, ClassName = F, CreatedTime = F, IsEncrypted = F, ModifiedTime = F, Name = F, SchemaFieldUniqueID = F, SchemaObjectUniqueID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, Value = F, WorkflowInstanceID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "WorkflowInstanceVariableID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Workflow", objectName = "WorkflowInstanceVariable", objectId = WorkflowInstanceVariableID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a WorkflowInstanceVariable
	#'
	#' This function deletes a WorkflowInstanceVariable
	#' @param WorkflowInstanceVariableID The ID of the WorkflowInstanceVariable to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The WorkflowInstanceVariableID of the deleted WorkflowInstanceVariable.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteWorkflowInstanceVariable <- function(WorkflowInstanceVariableID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Workflow", objectName = "WorkflowInstanceVariable", objectId = WorkflowInstanceVariableID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a WorkflowInstanceVariable
	#'
	#' This function creates a WorkflowInstanceVariable
	#' @param fieldNames The field values to give the created WorkflowInstanceVariable. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A newly created WorkflowInstanceVariable
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createWorkflowInstanceVariable <- function(ClassName = NULL, IsEncrypted = NULL, Name = NULL, Value = NULL, WorkflowInstanceID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Workflow", objectName = "WorkflowInstanceVariable", body = list(DataObject = body), searchFields = append("WorkflowInstanceVariableID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a WorkflowInstanceVariable
	#'
	#' This function modifies a WorkflowInstanceVariable
	#' @param fieldNames The field values to give the modified WorkflowInstanceVariable. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The modified WorkflowInstanceVariable
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyWorkflowInstanceVariable <- function(WorkflowInstanceVariableID, ClassName = NULL, IsEncrypted = NULL, Name = NULL, Value = NULL, WorkflowInstanceID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Workflow", objectName = "WorkflowInstanceVariable", objectId = WorkflowInstanceVariableID, body = list(DataObject = body), searchFields = append("WorkflowInstanceVariableID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List WorkflowVersions
	#'
	#' This function returns a dataframe or json object of WorkflowVersions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given WorkflowVersions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the WorkflowVersions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('WorkflowVersion') to get more field paths.
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
	#' @concept Workflow
	#' @return A list of WorkflowVersions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listWorkflowVersions <- function(searchConditionsList = NULL, AliasAction = F, AliasModule = F, AliasObject = F, CreatedTime = F, DataObjectDefaultSaveFlags = F, DataObjectPropertyDefaults = F, Description = F, ExpirationDurationCode = F, ExpirationDurationCustomDays = F, FullAlias = F, HasWorkflowInstances = F, IsCurrentWorkflowVersion = F, IsSkywardWorkflowVersion = F, ModifiedTime = F, NavigationGroups = F, Notes = F, Parameters = F, Portal = F, Queue = F, RefreshScreen = F, Scope = F, ScreenAttribute = F, SkywardHash = F, TaskID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, Variables = F, VariablesForClient = F, WorkflowID = F, WorkflowVersionID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "WorkflowVersion", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a WorkflowVersion
	#'
	#' This function returns a dataframe or json object of a WorkflowVersion
	#' @param WorkflowVersionID The ID of the WorkflowVersion to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given WorkflowVersion. Defaults to FALSE for all return fields which, for convenience, returns all fields for the WorkflowVersion.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('WorkflowVersion') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A dataframe or of WorkflowVersion
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getWorkflowVersion <- function(WorkflowVersionID, AliasAction = F, AliasModule = F, AliasObject = F, CreatedTime = F, DataObjectDefaultSaveFlags = F, DataObjectPropertyDefaults = F, Description = F, ExpirationDurationCode = F, ExpirationDurationCustomDays = F, FullAlias = F, HasWorkflowInstances = F, IsCurrentWorkflowVersion = F, IsSkywardWorkflowVersion = F, ModifiedTime = F, NavigationGroups = F, Notes = F, Parameters = F, Portal = F, Queue = F, RefreshScreen = F, Scope = F, ScreenAttribute = F, SkywardHash = F, TaskID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, Variables = F, VariablesForClient = F, WorkflowID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "WorkflowVersionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Workflow", objectName = "WorkflowVersion", objectId = WorkflowVersionID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a WorkflowVersion
	#'
	#' This function deletes a WorkflowVersion
	#' @param WorkflowVersionID The ID of the WorkflowVersion to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The WorkflowVersionID of the deleted WorkflowVersion.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteWorkflowVersion <- function(WorkflowVersionID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Workflow", objectName = "WorkflowVersion", objectId = WorkflowVersionID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a WorkflowVersion
	#'
	#' This function creates a WorkflowVersion
	#' @param fieldNames The field values to give the created WorkflowVersion. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A newly created WorkflowVersion
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createWorkflowVersion <- function(AliasAction = NULL, AliasModule = NULL, AliasObject = NULL, Description = NULL, ExpirationDurationCode = NULL, ExpirationDurationCustomDays = NULL, Notes = NULL, Queue = NULL, Scope = NULL, TaskID = NULL, WorkflowID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Workflow", objectName = "WorkflowVersion", body = list(DataObject = body), searchFields = append("WorkflowVersionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a WorkflowVersion
	#'
	#' This function modifies a WorkflowVersion
	#' @param fieldNames The field values to give the modified WorkflowVersion. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The modified WorkflowVersion
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyWorkflowVersion <- function(WorkflowVersionID, AliasAction = NULL, AliasModule = NULL, AliasObject = NULL, Description = NULL, ExpirationDurationCode = NULL, ExpirationDurationCustomDays = NULL, Notes = NULL, Queue = NULL, Scope = NULL, TaskID = NULL, WorkflowID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Workflow", objectName = "WorkflowVersion", objectId = WorkflowVersionID, body = list(DataObject = body), searchFields = append("WorkflowVersionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List WorkflowLocks
	#'
	#' This function returns a dataframe or json object of WorkflowLocks
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given WorkflowLocks. Defaults to FALSE for all return fields which, for convenience, returns all fields for the WorkflowLocks.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('WorkflowLock') to get more field paths.
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
	#' @concept Workflow
	#' @return A list of WorkflowLocks
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listWorkflowLocks <- function(searchConditionsList = NULL, CreatedTime = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, WorkflowInstanceID = F, WorkflowLockID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "WorkflowLock", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a WorkflowLock
	#'
	#' This function returns a dataframe or json object of a WorkflowLock
	#' @param WorkflowLockID The ID of the WorkflowLock to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given WorkflowLock. Defaults to FALSE for all return fields which, for convenience, returns all fields for the WorkflowLock.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('WorkflowLock') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A dataframe or of WorkflowLock
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getWorkflowLock <- function(WorkflowLockID, CreatedTime = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, WorkflowInstanceID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "WorkflowLockID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Workflow", objectName = "WorkflowLock", objectId = WorkflowLockID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a WorkflowLock
	#'
	#' This function deletes a WorkflowLock
	#' @param WorkflowLockID The ID of the WorkflowLock to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The WorkflowLockID of the deleted WorkflowLock.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteWorkflowLock <- function(WorkflowLockID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Workflow", objectName = "WorkflowLock", objectId = WorkflowLockID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a WorkflowLock
	#'
	#' This function creates a WorkflowLock
	#' @param fieldNames The field values to give the created WorkflowLock. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A newly created WorkflowLock
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createWorkflowLock <- function(WorkflowInstanceID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Workflow", objectName = "WorkflowLock", body = list(DataObject = body), searchFields = append("WorkflowLockID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a WorkflowLock
	#'
	#' This function modifies a WorkflowLock
	#' @param fieldNames The field values to give the modified WorkflowLock. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The modified WorkflowLock
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyWorkflowLock <- function(WorkflowLockID, WorkflowInstanceID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Workflow", objectName = "WorkflowLock", objectId = WorkflowLockID, body = list(DataObject = body), searchFields = append("WorkflowLockID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ApprovalTaskIdentifiers
	#'
	#' This function returns a dataframe or json object of ApprovalTaskIdentifiers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ApprovalTaskIdentifiers. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ApprovalTaskIdentifiers.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ApprovalTaskIdentifier') to get more field paths.
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
	#' @concept Workflow
	#' @return A list of ApprovalTaskIdentifiers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listApprovalTaskIdentifiers <- function(searchConditionsList = NULL, ApprovalTaskIdentifierID = F, ApprovalType = F, ApprovalTypeCode = F, CreatedTime = F, Description = F, FilterData = F, IsConditional = F, Level = F, ModifiedTime = F, TaskID = F, TaskType = F, TaskTypeCode = F, TypedApprovalTaskID = F, UseOrganizationChart = F, UserIDCreatedBy = F, UserIDModifiedBy = F, WorkflowVersionID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "ApprovalTaskIdentifier", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ApprovalTaskIdentifier
	#'
	#' This function returns a dataframe or json object of an ApprovalTaskIdentifier
	#' @param ApprovalTaskIdentifierID The ID of the ApprovalTaskIdentifier to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ApprovalTaskIdentifier. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ApprovalTaskIdentifier.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ApprovalTaskIdentifier') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A dataframe or of ApprovalTaskIdentifier
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getApprovalTaskIdentifier <- function(ApprovalTaskIdentifierID, ApprovalType = F, ApprovalTypeCode = F, CreatedTime = F, Description = F, FilterData = F, IsConditional = F, Level = F, ModifiedTime = F, TaskID = F, TaskType = F, TaskTypeCode = F, TypedApprovalTaskID = F, UseOrganizationChart = F, UserIDCreatedBy = F, UserIDModifiedBy = F, WorkflowVersionID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ApprovalTaskIdentifierID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Workflow", objectName = "ApprovalTaskIdentifier", objectId = ApprovalTaskIdentifierID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ApprovalTaskIdentifier
	#'
	#' This function deletes an ApprovalTaskIdentifier
	#' @param ApprovalTaskIdentifierID The ID of the ApprovalTaskIdentifier to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The ApprovalTaskIdentifierID of the deleted ApprovalTaskIdentifier.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteApprovalTaskIdentifier <- function(ApprovalTaskIdentifierID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Workflow", objectName = "ApprovalTaskIdentifier", objectId = ApprovalTaskIdentifierID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ApprovalTaskIdentifier
	#'
	#' This function creates an ApprovalTaskIdentifier
	#' @param fieldNames The field values to give the created ApprovalTaskIdentifier. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A newly created ApprovalTaskIdentifier
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createApprovalTaskIdentifier <- function(ApprovalType = NULL, Description = NULL, FilterData = NULL, IsConditional = NULL, Level = NULL, TaskID = NULL, TaskType = NULL, TypedApprovalTaskID = NULL, UseOrganizationChart = NULL, WorkflowVersionID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Workflow", objectName = "ApprovalTaskIdentifier", body = list(DataObject = body), searchFields = append("ApprovalTaskIdentifierID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ApprovalTaskIdentifier
	#'
	#' This function modifies an ApprovalTaskIdentifier
	#' @param fieldNames The field values to give the modified ApprovalTaskIdentifier. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The modified ApprovalTaskIdentifier
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyApprovalTaskIdentifier <- function(ApprovalTaskIdentifierID, ApprovalType = NULL, Description = NULL, FilterData = NULL, IsConditional = NULL, Level = NULL, TaskID = NULL, TaskType = NULL, TypedApprovalTaskID = NULL, UseOrganizationChart = NULL, WorkflowVersionID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Workflow", objectName = "ApprovalTaskIdentifier", objectId = ApprovalTaskIdentifierID, body = list(DataObject = body), searchFields = append("ApprovalTaskIdentifierID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ApprovalTaskPresenters
	#'
	#' This function returns a dataframe or json object of ApprovalTaskPresenters
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ApprovalTaskPresenters. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ApprovalTaskPresenters.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ApprovalTaskPresenter') to get more field paths.
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
	#' @concept Workflow
	#' @return A list of ApprovalTaskPresenters
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listApprovalTaskPresenters <- function(searchConditionsList = NULL, ApprovalObjectID = F, ApprovalTaskPresenterID = F, ApprovalType = F, ApprovalTypeCode = F, CreatedTime = F, Description = F, ModifiedTime = F, TaskInstanceID = F, Type = F, UserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "ApprovalTaskPresenter", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ApprovalTaskPresenter
	#'
	#' This function returns a dataframe or json object of an ApprovalTaskPresenter
	#' @param ApprovalTaskPresenterID The ID of the ApprovalTaskPresenter to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ApprovalTaskPresenter. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ApprovalTaskPresenter.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ApprovalTaskPresenter') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A dataframe or of ApprovalTaskPresenter
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getApprovalTaskPresenter <- function(ApprovalTaskPresenterID, ApprovalObjectID = F, ApprovalType = F, ApprovalTypeCode = F, CreatedTime = F, Description = F, ModifiedTime = F, TaskInstanceID = F, Type = F, UserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ApprovalTaskPresenterID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Workflow", objectName = "ApprovalTaskPresenter", objectId = ApprovalTaskPresenterID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ApprovalTaskPresenter
	#'
	#' This function deletes an ApprovalTaskPresenter
	#' @param ApprovalTaskPresenterID The ID of the ApprovalTaskPresenter to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The ApprovalTaskPresenterID of the deleted ApprovalTaskPresenter.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteApprovalTaskPresenter <- function(ApprovalTaskPresenterID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Workflow", objectName = "ApprovalTaskPresenter", objectId = ApprovalTaskPresenterID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ApprovalTaskPresenter
	#'
	#' This function creates an ApprovalTaskPresenter
	#' @param fieldNames The field values to give the created ApprovalTaskPresenter. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A newly created ApprovalTaskPresenter
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createApprovalTaskPresenter <- function(ApprovalObjectID = NULL, ApprovalType = NULL, Description = NULL, TaskInstanceID = NULL, Type = NULL, UserID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Workflow", objectName = "ApprovalTaskPresenter", body = list(DataObject = body), searchFields = append("ApprovalTaskPresenterID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ApprovalTaskPresenter
	#'
	#' This function modifies an ApprovalTaskPresenter
	#' @param fieldNames The field values to give the modified ApprovalTaskPresenter. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The modified ApprovalTaskPresenter
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyApprovalTaskPresenter <- function(ApprovalTaskPresenterID, ApprovalObjectID = NULL, ApprovalType = NULL, Description = NULL, TaskInstanceID = NULL, Type = NULL, UserID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Workflow", objectName = "ApprovalTaskPresenter", objectId = ApprovalTaskPresenterID, body = list(DataObject = body), searchFields = append("ApprovalTaskPresenterID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ApprovalWorkflowLocks
	#'
	#' This function returns a dataframe or json object of ApprovalWorkflowLocks
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ApprovalWorkflowLocks. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ApprovalWorkflowLocks.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ApprovalWorkflowLock') to get more field paths.
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
	#' @concept Workflow
	#' @return A list of ApprovalWorkflowLocks
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listApprovalWorkflowLocks <- function(searchConditionsList = NULL, ApprovalObjectID = F, ApprovalType = F, ApprovalTypeCode = F, ApprovalWorkflowLockID = F, CreatedTime = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "ApprovalWorkflowLock", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ApprovalWorkflowLock
	#'
	#' This function returns a dataframe or json object of an ApprovalWorkflowLock
	#' @param ApprovalWorkflowLockID The ID of the ApprovalWorkflowLock to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ApprovalWorkflowLock. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ApprovalWorkflowLock.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ApprovalWorkflowLock') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A dataframe or of ApprovalWorkflowLock
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getApprovalWorkflowLock <- function(ApprovalWorkflowLockID, ApprovalObjectID = F, ApprovalType = F, ApprovalTypeCode = F, CreatedTime = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ApprovalWorkflowLockID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Workflow", objectName = "ApprovalWorkflowLock", objectId = ApprovalWorkflowLockID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ApprovalWorkflowLock
	#'
	#' This function deletes an ApprovalWorkflowLock
	#' @param ApprovalWorkflowLockID The ID of the ApprovalWorkflowLock to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The ApprovalWorkflowLockID of the deleted ApprovalWorkflowLock.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteApprovalWorkflowLock <- function(ApprovalWorkflowLockID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Workflow", objectName = "ApprovalWorkflowLock", objectId = ApprovalWorkflowLockID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ApprovalWorkflowLock
	#'
	#' This function creates an ApprovalWorkflowLock
	#' @param fieldNames The field values to give the created ApprovalWorkflowLock. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A newly created ApprovalWorkflowLock
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createApprovalWorkflowLock <- function(ApprovalObjectID = NULL, ApprovalType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Workflow", objectName = "ApprovalWorkflowLock", body = list(DataObject = body), searchFields = append("ApprovalWorkflowLockID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ApprovalWorkflowLock
	#'
	#' This function modifies an ApprovalWorkflowLock
	#' @param fieldNames The field values to give the modified ApprovalWorkflowLock. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The modified ApprovalWorkflowLock
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyApprovalWorkflowLock <- function(ApprovalWorkflowLockID, ApprovalObjectID = NULL, ApprovalType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Workflow", objectName = "ApprovalWorkflowLock", objectId = ApprovalWorkflowLockID, body = list(DataObject = body), searchFields = append("ApprovalWorkflowLockID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ApprovalTaskClearances
	#'
	#' This function returns a dataframe or json object of ApprovalTaskClearances
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ApprovalTaskClearances. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ApprovalTaskClearances.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ApprovalTaskClearance') to get more field paths.
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
	#' @concept Workflow
	#' @return A list of ApprovalTaskClearances
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listApprovalTaskClearances <- function(searchConditionsList = NULL, ApprovalTaskClearanceID = F, ApprovalTaskIdentifierID = F, CreatedTime = F, GroupIDSecurity = F, ModifiedTime = F, Type = F, TypeCode = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "ApprovalTaskClearance", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ApprovalTaskClearance
	#'
	#' This function returns a dataframe or json object of an ApprovalTaskClearance
	#' @param ApprovalTaskClearanceID The ID of the ApprovalTaskClearance to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ApprovalTaskClearance. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ApprovalTaskClearance.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ApprovalTaskClearance') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A dataframe or of ApprovalTaskClearance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getApprovalTaskClearance <- function(ApprovalTaskClearanceID, ApprovalTaskIdentifierID = F, CreatedTime = F, GroupIDSecurity = F, ModifiedTime = F, Type = F, TypeCode = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ApprovalTaskClearanceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Workflow", objectName = "ApprovalTaskClearance", objectId = ApprovalTaskClearanceID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ApprovalTaskClearance
	#'
	#' This function deletes an ApprovalTaskClearance
	#' @param ApprovalTaskClearanceID The ID of the ApprovalTaskClearance to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The ApprovalTaskClearanceID of the deleted ApprovalTaskClearance.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteApprovalTaskClearance <- function(ApprovalTaskClearanceID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Workflow", objectName = "ApprovalTaskClearance", objectId = ApprovalTaskClearanceID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ApprovalTaskClearance
	#'
	#' This function creates an ApprovalTaskClearance
	#' @param fieldNames The field values to give the created ApprovalTaskClearance. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A newly created ApprovalTaskClearance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createApprovalTaskClearance <- function(ApprovalTaskIdentifierID = NULL, GroupIDSecurity = NULL, Type = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Workflow", objectName = "ApprovalTaskClearance", body = list(DataObject = body), searchFields = append("ApprovalTaskClearanceID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ApprovalTaskClearance
	#'
	#' This function modifies an ApprovalTaskClearance
	#' @param fieldNames The field values to give the modified ApprovalTaskClearance. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The modified ApprovalTaskClearance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyApprovalTaskClearance <- function(ApprovalTaskClearanceID, ApprovalTaskIdentifierID = NULL, GroupIDSecurity = NULL, Type = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Workflow", objectName = "ApprovalTaskClearance", objectId = ApprovalTaskClearanceID, body = list(DataObject = body), searchFields = append("ApprovalTaskClearanceID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ApprovalWorkflowInstances
	#'
	#' This function returns a dataframe or json object of ApprovalWorkflowInstances
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ApprovalWorkflowInstances. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ApprovalWorkflowInstances.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ApprovalWorkflowInstance') to get more field paths.
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
	#' @concept Workflow
	#' @return A list of ApprovalWorkflowInstances
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listApprovalWorkflowInstances <- function(searchConditionsList = NULL, ApprovalObjectID = F, ApprovalType = F, ApprovalTypeCode = F, ApprovalWorkflowInstanceID = F, CreatedTime = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, WorkflowInstanceID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "ApprovalWorkflowInstance", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ApprovalWorkflowInstance
	#'
	#' This function returns a dataframe or json object of an ApprovalWorkflowInstance
	#' @param ApprovalWorkflowInstanceID The ID of the ApprovalWorkflowInstance to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ApprovalWorkflowInstance. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ApprovalWorkflowInstance.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ApprovalWorkflowInstance') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A dataframe or of ApprovalWorkflowInstance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getApprovalWorkflowInstance <- function(ApprovalWorkflowInstanceID, ApprovalObjectID = F, ApprovalType = F, ApprovalTypeCode = F, CreatedTime = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, WorkflowInstanceID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ApprovalWorkflowInstanceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Workflow", objectName = "ApprovalWorkflowInstance", objectId = ApprovalWorkflowInstanceID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ApprovalWorkflowInstance
	#'
	#' This function deletes an ApprovalWorkflowInstance
	#' @param ApprovalWorkflowInstanceID The ID of the ApprovalWorkflowInstance to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The ApprovalWorkflowInstanceID of the deleted ApprovalWorkflowInstance.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteApprovalWorkflowInstance <- function(ApprovalWorkflowInstanceID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Workflow", objectName = "ApprovalWorkflowInstance", objectId = ApprovalWorkflowInstanceID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ApprovalWorkflowInstance
	#'
	#' This function creates an ApprovalWorkflowInstance
	#' @param fieldNames The field values to give the created ApprovalWorkflowInstance. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A newly created ApprovalWorkflowInstance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createApprovalWorkflowInstance <- function(ApprovalObjectID = NULL, ApprovalType = NULL, WorkflowInstanceID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Workflow", objectName = "ApprovalWorkflowInstance", body = list(DataObject = body), searchFields = append("ApprovalWorkflowInstanceID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ApprovalWorkflowInstance
	#'
	#' This function modifies an ApprovalWorkflowInstance
	#' @param fieldNames The field values to give the modified ApprovalWorkflowInstance. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The modified ApprovalWorkflowInstance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyApprovalWorkflowInstance <- function(ApprovalWorkflowInstanceID, ApprovalObjectID = NULL, ApprovalType = NULL, WorkflowInstanceID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Workflow", objectName = "ApprovalWorkflowInstance", objectId = ApprovalWorkflowInstanceID, body = list(DataObject = body), searchFields = append("ApprovalWorkflowInstanceID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Templates
	#'
	#' This function returns a dataframe or json object of Templates
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Templates. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Templates.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Template') to get more field paths.
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
	#' @concept Workflow
	#' @return A list of Templates
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTemplates <- function(searchConditionsList = NULL, Controls = F, CreatedTime = F, Data = F, Description = F, DistrictID = F, EntityID = F, FiscalYearID = F, IsClonable = F, IsDefault = F, IsDeleteable = F, IsEditable = F, IsSkywardDefault = F, IsVisible = F, ModifiedTime = F, Name = F, Published = F, SchoolYearID = F, TaskName = F, TemplateID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserIDOwner = F, WorkflowID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "Template", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Template
	#'
	#' This function returns a dataframe or json object of a Template
	#' @param TemplateID The ID of the Template to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Template. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Template.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Template') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A dataframe or of Template
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTemplate <- function(TemplateID, Controls = F, CreatedTime = F, Data = F, Description = F, DistrictID = F, EntityID = F, FiscalYearID = F, IsClonable = F, IsDefault = F, IsDeleteable = F, IsEditable = F, IsSkywardDefault = F, IsVisible = F, ModifiedTime = F, Name = F, Published = F, SchoolYearID = F, TaskName = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserIDOwner = F, WorkflowID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TemplateID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Workflow", objectName = "Template", objectId = TemplateID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Template
	#'
	#' This function deletes a Template
	#' @param TemplateID The ID of the Template to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The TemplateID of the deleted Template.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTemplate <- function(TemplateID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Workflow", objectName = "Template", objectId = TemplateID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Template
	#'
	#' This function creates a Template
	#' @param fieldNames The field values to give the created Template. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A newly created Template
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTemplate <- function(Data = NULL, Description = NULL, DistrictID = NULL, EntityID = NULL, FiscalYearID = NULL, Name = NULL, Published = NULL, SchoolYearID = NULL, TaskName = NULL, UserIDOwner = NULL, WorkflowID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Workflow", objectName = "Template", body = list(DataObject = body), searchFields = append("TemplateID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Template
	#'
	#' This function modifies a Template
	#' @param fieldNames The field values to give the modified Template. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The modified Template
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTemplate <- function(TemplateID, Data = NULL, Description = NULL, DistrictID = NULL, EntityID = NULL, FiscalYearID = NULL, Name = NULL, Published = NULL, SchoolYearID = NULL, TaskName = NULL, UserIDOwner = NULL, WorkflowID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Workflow", objectName = "Template", objectId = TemplateID, body = list(DataObject = body), searchFields = append("TemplateID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TemplateDefaults
	#'
	#' This function returns a dataframe or json object of TemplateDefaults
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TemplateDefaults. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TemplateDefaults.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TemplateDefault') to get more field paths.
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
	#' @concept Workflow
	#' @return A list of TemplateDefaults
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTemplateDefaults <- function(searchConditionsList = NULL, CreatedTime = F, ModifiedTime = F, TemplateDefaultID = F, TemplateID = F, UserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "TemplateDefault", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TemplateDefault
	#'
	#' This function returns a dataframe or json object of a TemplateDefault
	#' @param TemplateDefaultID The ID of the TemplateDefault to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TemplateDefault. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TemplateDefault.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TemplateDefault') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A dataframe or of TemplateDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTemplateDefault <- function(TemplateDefaultID, CreatedTime = F, ModifiedTime = F, TemplateID = F, UserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TemplateDefaultID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Workflow", objectName = "TemplateDefault", objectId = TemplateDefaultID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TemplateDefault
	#'
	#' This function deletes a TemplateDefault
	#' @param TemplateDefaultID The ID of the TemplateDefault to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The TemplateDefaultID of the deleted TemplateDefault.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTemplateDefault <- function(TemplateDefaultID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Workflow", objectName = "TemplateDefault", objectId = TemplateDefaultID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TemplateDefault
	#'
	#' This function creates a TemplateDefault
	#' @param fieldNames The field values to give the created TemplateDefault. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A newly created TemplateDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTemplateDefault <- function(TemplateID = NULL, UserID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Workflow", objectName = "TemplateDefault", body = list(DataObject = body), searchFields = append("TemplateDefaultID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TemplateDefault
	#'
	#' This function modifies a TemplateDefault
	#' @param fieldNames The field values to give the modified TemplateDefault. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The modified TemplateDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTemplateDefault <- function(TemplateDefaultID, TemplateID = NULL, UserID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Workflow", objectName = "TemplateDefault", objectId = TemplateDefaultID, body = list(DataObject = body), searchFields = append("TemplateDefaultID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TemplatePreviousValues
	#'
	#' This function returns a dataframe or json object of TemplatePreviousValues
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TemplatePreviousValues. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TemplatePreviousValues.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TemplatePreviousValue') to get more field paths.
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
	#' @concept Workflow
	#' @return A list of TemplatePreviousValues
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTemplatePreviousValues <- function(searchConditionsList = NULL, Controls = F, CreatedTime = F, Data = F, ModifiedTime = F, TemplateID = F, TemplatePreviousValueID = F, UserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "TemplatePreviousValue", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TemplatePreviousValue
	#'
	#' This function returns a dataframe or json object of a TemplatePreviousValue
	#' @param TemplatePreviousValueID The ID of the TemplatePreviousValue to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TemplatePreviousValue. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TemplatePreviousValue.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TemplatePreviousValue') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A dataframe or of TemplatePreviousValue
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTemplatePreviousValue <- function(TemplatePreviousValueID, Controls = F, CreatedTime = F, Data = F, ModifiedTime = F, TemplateID = F, UserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TemplatePreviousValueID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Workflow", objectName = "TemplatePreviousValue", objectId = TemplatePreviousValueID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TemplatePreviousValue
	#'
	#' This function deletes a TemplatePreviousValue
	#' @param TemplatePreviousValueID The ID of the TemplatePreviousValue to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The TemplatePreviousValueID of the deleted TemplatePreviousValue.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTemplatePreviousValue <- function(TemplatePreviousValueID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Workflow", objectName = "TemplatePreviousValue", objectId = TemplatePreviousValueID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TemplatePreviousValue
	#'
	#' This function creates a TemplatePreviousValue
	#' @param fieldNames The field values to give the created TemplatePreviousValue. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A newly created TemplatePreviousValue
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTemplatePreviousValue <- function(Data = NULL, TemplateID = NULL, UserID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Workflow", objectName = "TemplatePreviousValue", body = list(DataObject = body), searchFields = append("TemplatePreviousValueID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TemplatePreviousValue
	#'
	#' This function modifies a TemplatePreviousValue
	#' @param fieldNames The field values to give the modified TemplatePreviousValue. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The modified TemplatePreviousValue
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTemplatePreviousValue <- function(TemplatePreviousValueID, Data = NULL, TemplateID = NULL, UserID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Workflow", objectName = "TemplatePreviousValue", objectId = TemplatePreviousValueID, body = list(DataObject = body), searchFields = append("TemplatePreviousValueID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TemplateLastUseds
	#'
	#' This function returns a dataframe or json object of TemplateLastUseds
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TemplateLastUseds. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TemplateLastUseds.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TemplateLastUsed') to get more field paths.
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
	#' @concept Workflow
	#' @return A list of TemplateLastUseds
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTemplateLastUseds <- function(searchConditionsList = NULL, CreatedTime = F, ModifiedTime = F, TemplateID = F, TemplateLastUsedID = F, UserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "TemplateLastUsed", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TemplateLastUsed
	#'
	#' This function returns a dataframe or json object of a TemplateLastUsed
	#' @param TemplateLastUsedID The ID of the TemplateLastUsed to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TemplateLastUsed. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TemplateLastUsed.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TemplateLastUsed') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A dataframe or of TemplateLastUsed
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTemplateLastUsed <- function(TemplateLastUsedID, CreatedTime = F, ModifiedTime = F, TemplateID = F, UserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TemplateLastUsedID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Workflow", objectName = "TemplateLastUsed", objectId = TemplateLastUsedID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TemplateLastUsed
	#'
	#' This function deletes a TemplateLastUsed
	#' @param TemplateLastUsedID The ID of the TemplateLastUsed to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The TemplateLastUsedID of the deleted TemplateLastUsed.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTemplateLastUsed <- function(TemplateLastUsedID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Workflow", objectName = "TemplateLastUsed", objectId = TemplateLastUsedID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TemplateLastUsed
	#'
	#' This function creates a TemplateLastUsed
	#' @param fieldNames The field values to give the created TemplateLastUsed. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A newly created TemplateLastUsed
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTemplateLastUsed <- function(TemplateID = NULL, UserID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Workflow", objectName = "TemplateLastUsed", body = list(DataObject = body), searchFields = append("TemplateLastUsedID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TemplateLastUsed
	#'
	#' This function modifies a TemplateLastUsed
	#' @param fieldNames The field values to give the modified TemplateLastUsed. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The modified TemplateLastUsed
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTemplateLastUsed <- function(TemplateLastUsedID, TemplateID = NULL, UserID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Workflow", objectName = "TemplateLastUsed", objectId = TemplateLastUsedID, body = list(DataObject = body), searchFields = append("TemplateLastUsedID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SynchronousLocks
	#'
	#' This function returns a dataframe or json object of SynchronousLocks
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SynchronousLocks. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SynchronousLocks.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SynchronousLock') to get more field paths.
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
	#' @concept Workflow
	#' @return A list of SynchronousLocks
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSynchronousLocks <- function(searchConditionsList = NULL, CreatedTime = F, LockingKey = F, ModifiedTime = F, SynchronousLockID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, Waiting = F, WorkflowID = F, WorkflowInstanceID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "SynchronousLock", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SynchronousLock
	#'
	#' This function returns a dataframe or json object of a SynchronousLock
	#' @param SynchronousLockID The ID of the SynchronousLock to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SynchronousLock. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SynchronousLock.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SynchronousLock') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A dataframe or of SynchronousLock
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSynchronousLock <- function(SynchronousLockID, CreatedTime = F, LockingKey = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, Waiting = F, WorkflowID = F, WorkflowInstanceID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SynchronousLockID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Workflow", objectName = "SynchronousLock", objectId = SynchronousLockID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SynchronousLock
	#'
	#' This function deletes a SynchronousLock
	#' @param SynchronousLockID The ID of the SynchronousLock to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The SynchronousLockID of the deleted SynchronousLock.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSynchronousLock <- function(SynchronousLockID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Workflow", objectName = "SynchronousLock", objectId = SynchronousLockID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SynchronousLock
	#'
	#' This function creates a SynchronousLock
	#' @param fieldNames The field values to give the created SynchronousLock. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A newly created SynchronousLock
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSynchronousLock <- function(LockingKey = NULL, Waiting = NULL, WorkflowID = NULL, WorkflowInstanceID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Workflow", objectName = "SynchronousLock", body = list(DataObject = body), searchFields = append("SynchronousLockID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SynchronousLock
	#'
	#' This function modifies a SynchronousLock
	#' @param fieldNames The field values to give the modified SynchronousLock. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The modified SynchronousLock
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySynchronousLock <- function(SynchronousLockID, LockingKey = NULL, Waiting = NULL, WorkflowID = NULL, WorkflowInstanceID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Workflow", objectName = "SynchronousLock", objectId = SynchronousLockID, body = list(DataObject = body), searchFields = append("SynchronousLockID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ApprovalSupervisors
	#'
	#' This function returns a dataframe or json object of ApprovalSupervisors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ApprovalSupervisors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ApprovalSupervisors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ApprovalSupervisor') to get more field paths.
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
	#' @concept Workflow
	#' @return A list of ApprovalSupervisors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listApprovalSupervisors <- function(searchConditionsList = NULL, ApprovalObjectID = F, ApprovalSupervisorID = F, ApprovalType = F, ApprovalTypeCode = F, CreatedTime = F, ModifiedTime = F, OrganizationChartRelationshipID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "ApprovalSupervisor", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ApprovalSupervisor
	#'
	#' This function returns a dataframe or json object of an ApprovalSupervisor
	#' @param ApprovalSupervisorID The ID of the ApprovalSupervisor to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ApprovalSupervisor. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ApprovalSupervisor.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ApprovalSupervisor') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A dataframe or of ApprovalSupervisor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getApprovalSupervisor <- function(ApprovalSupervisorID, ApprovalObjectID = F, ApprovalType = F, ApprovalTypeCode = F, CreatedTime = F, ModifiedTime = F, OrganizationChartRelationshipID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ApprovalSupervisorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Workflow", objectName = "ApprovalSupervisor", objectId = ApprovalSupervisorID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ApprovalSupervisor
	#'
	#' This function deletes an ApprovalSupervisor
	#' @param ApprovalSupervisorID The ID of the ApprovalSupervisor to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The ApprovalSupervisorID of the deleted ApprovalSupervisor.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteApprovalSupervisor <- function(ApprovalSupervisorID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Workflow", objectName = "ApprovalSupervisor", objectId = ApprovalSupervisorID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ApprovalSupervisor
	#'
	#' This function creates an ApprovalSupervisor
	#' @param fieldNames The field values to give the created ApprovalSupervisor. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A newly created ApprovalSupervisor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createApprovalSupervisor <- function(ApprovalObjectID = NULL, ApprovalType = NULL, OrganizationChartRelationshipID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Workflow", objectName = "ApprovalSupervisor", body = list(DataObject = body), searchFields = append("ApprovalSupervisorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ApprovalSupervisor
	#'
	#' This function modifies an ApprovalSupervisor
	#' @param fieldNames The field values to give the modified ApprovalSupervisor. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The modified ApprovalSupervisor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyApprovalSupervisor <- function(ApprovalSupervisorID, ApprovalObjectID = NULL, ApprovalType = NULL, OrganizationChartRelationshipID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Workflow", objectName = "ApprovalSupervisor", objectId = ApprovalSupervisorID, body = list(DataObject = body), searchFields = append("ApprovalSupervisorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempTemplateClones
	#'
	#' This function returns a dataframe or json object of TempTemplateClones
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempTemplateClones. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempTemplateClones.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempTemplateClone') to get more field paths.
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
	#' @concept Workflow
	#' @return A list of TempTemplateClones
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempTemplateClones <- function(searchConditionsList = NULL, CreatedTime = F, District = F, DistrictID = F, Entity = F, EntityID = F, FiscalYear = F, FiscalYearID = F, HasErrors = F, ModifiedTime = F, Name = F, SchoolYear = F, SchoolYearID = F, TaskName = F, TempTemplateCloneID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, WorkflowName = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "TempTemplateClone", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempTemplateClone
	#'
	#' This function returns a dataframe or json object of a TempTemplateClone
	#' @param TempTemplateCloneID The ID of the TempTemplateClone to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempTemplateClone. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempTemplateClone.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempTemplateClone') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A dataframe or of TempTemplateClone
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempTemplateClone <- function(TempTemplateCloneID, CreatedTime = F, District = F, DistrictID = F, Entity = F, EntityID = F, FiscalYear = F, FiscalYearID = F, HasErrors = F, ModifiedTime = F, Name = F, SchoolYear = F, SchoolYearID = F, TaskName = F, UserIDCreatedBy = F, UserIDModifiedBy = F, WorkflowName = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempTemplateCloneID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Workflow", objectName = "TempTemplateClone", objectId = TempTemplateCloneID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempTemplateClone
	#'
	#' This function deletes a TempTemplateClone
	#' @param TempTemplateCloneID The ID of the TempTemplateClone to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The TempTemplateCloneID of the deleted TempTemplateClone.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempTemplateClone <- function(TempTemplateCloneID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Workflow", objectName = "TempTemplateClone", objectId = TempTemplateCloneID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempTemplateClone
	#'
	#' This function creates a TempTemplateClone
	#' @param fieldNames The field values to give the created TempTemplateClone. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A newly created TempTemplateClone
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempTemplateClone <- function(District = NULL, DistrictID = NULL, Entity = NULL, EntityID = NULL, FiscalYear = NULL, FiscalYearID = NULL, HasErrors = NULL, Name = NULL, SchoolYear = NULL, SchoolYearID = NULL, TaskName = NULL, WorkflowName = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Workflow", objectName = "TempTemplateClone", body = list(DataObject = body), searchFields = append("TempTemplateCloneID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempTemplateClone
	#'
	#' This function modifies a TempTemplateClone
	#' @param fieldNames The field values to give the modified TempTemplateClone. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The modified TempTemplateClone
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempTemplateClone <- function(TempTemplateCloneID, District = NULL, DistrictID = NULL, Entity = NULL, EntityID = NULL, FiscalYear = NULL, FiscalYearID = NULL, HasErrors = NULL, Name = NULL, SchoolYear = NULL, SchoolYearID = NULL, TaskName = NULL, WorkflowName = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Workflow", objectName = "TempTemplateClone", objectId = TempTemplateCloneID, body = list(DataObject = body), searchFields = append("TempTemplateCloneID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempTemplateCloneErrors
	#'
	#' This function returns a dataframe or json object of TempTemplateCloneErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempTemplateCloneErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempTemplateCloneErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempTemplateCloneError') to get more field paths.
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
	#' @concept Workflow
	#' @return A list of TempTemplateCloneErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempTemplateCloneErrors <- function(searchConditionsList = NULL, CreatedTime = F, Field = F, Message = F, ModifiedTime = F, TempTemplateCloneErrorID = F, TempTemplateCloneID = F, Type = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Workflow", objectName = "TempTemplateCloneError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempTemplateCloneError
	#'
	#' This function returns a dataframe or json object of a TempTemplateCloneError
	#' @param TempTemplateCloneErrorID The ID of the TempTemplateCloneError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempTemplateCloneError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempTemplateCloneError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempTemplateCloneError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A dataframe or of TempTemplateCloneError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempTemplateCloneError <- function(TempTemplateCloneErrorID, CreatedTime = F, Field = F, Message = F, ModifiedTime = F, TempTemplateCloneID = F, Type = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempTemplateCloneErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Workflow", objectName = "TempTemplateCloneError", objectId = TempTemplateCloneErrorID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempTemplateCloneError
	#'
	#' This function deletes a TempTemplateCloneError
	#' @param TempTemplateCloneErrorID The ID of the TempTemplateCloneError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The TempTemplateCloneErrorID of the deleted TempTemplateCloneError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempTemplateCloneError <- function(TempTemplateCloneErrorID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Workflow", objectName = "TempTemplateCloneError", objectId = TempTemplateCloneErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempTemplateCloneError
	#'
	#' This function creates a TempTemplateCloneError
	#' @param fieldNames The field values to give the created TempTemplateCloneError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return A newly created TempTemplateCloneError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempTemplateCloneError <- function(Field = NULL, Message = NULL, TempTemplateCloneID = NULL, Type = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Workflow", objectName = "TempTemplateCloneError", body = list(DataObject = body), searchFields = append("TempTemplateCloneErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempTemplateCloneError
	#'
	#' This function modifies a TempTemplateCloneError
	#' @param fieldNames The field values to give the modified TempTemplateCloneError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Workflow
	#' @return The modified TempTemplateCloneError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempTemplateCloneError <- function(TempTemplateCloneErrorID, Field = NULL, Message = NULL, TempTemplateCloneID = NULL, Type = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Workflow", objectName = "TempTemplateCloneError", objectId = TempTemplateCloneErrorID, body = list(DataObject = body), searchFields = append("TempTemplateCloneErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}
