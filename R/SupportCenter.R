
	#' List SupportCenterAttachments
	#'
	#' This function returns a dataframe or json object of SupportCenterAttachments
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SupportCenterAttachments. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SupportCenterAttachments.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SupportCenterAttachment') to get more field paths.
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
	#' @concept SupportCenter
	#' @return A list of SupportCenterAttachments
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSupportCenterAttachments <- function(searchConditionsList = NULL, ActionID = F, AttachmentID = F, Created = F, CreatedTime = F, MediaID = F, ModifiedTime = F, Name = F, ProjectID = F, RMSAttachmentID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SupportCenter", objectName = "Attachment", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SupportCenterAttachment
	#'
	#' This function returns a dataframe or json object of a SupportCenterAttachment
	#' @param SupportCenterAttachmentID The ID of the SupportCenterAttachment to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SupportCenterAttachment. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SupportCenterAttachment.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SupportCenterAttachment') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SupportCenter
	#' @return A dataframe or of SupportCenterAttachment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSupportCenterAttachment <- function(SupportCenterAttachmentID, ActionID = F, AttachmentID = F, Created = F, CreatedTime = F, MediaID = F, ModifiedTime = F, Name = F, ProjectID = F, RMSAttachmentID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SupportCenterAttachmentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SupportCenter", objectName = "Attachment", objectId = SupportCenterAttachmentID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SupportCenterAttachment
	#'
	#' This function deletes a SupportCenterAttachment
	#' @param SupportCenterAttachmentID The ID of the SupportCenterAttachment to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SupportCenter
	#' @return The SupportCenterAttachmentID of the deleted SupportCenterAttachment.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSupportCenterAttachment <- function(SupportCenterAttachmentID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SupportCenter", objectName = "Attachment", objectId = SupportCenterAttachmentID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SupportCenterAttachment
	#'
	#' This function creates a SupportCenterAttachment
	#' @param fieldNames The field values to give the created SupportCenterAttachment. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SupportCenter
	#' @return A newly created SupportCenterAttachment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSupportCenterAttachment <- function(ActionID = NULL, Created = NULL, MediaID = NULL, Name = NULL, ProjectID = NULL, RMSAttachmentID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SupportCenter", objectName = "Attachment", body = list(DataObject = body), searchFields = append("AttachmentID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SupportCenterAttachment
	#'
	#' This function modifies a SupportCenterAttachment
	#' @param fieldNames The field values to give the modified SupportCenterAttachment. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SupportCenter
	#' @return The modified SupportCenterAttachment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySupportCenterAttachment <- function(AttachmentID, ActionID = NULL, Created = NULL, MediaID = NULL, Name = NULL, ProjectID = NULL, RMSAttachmentID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SupportCenter", objectName = "Attachment", objectId = AttachmentID, body = list(DataObject = body), searchFields = append("AttachmentID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SupportCenterActions
	#'
	#' This function returns a dataframe or json object of SupportCenterActions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SupportCenterActions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SupportCenterActions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SupportCenterAction') to get more field paths.
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
	#' @concept SupportCenter
	#' @return A list of SupportCenterActions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSupportCenterActions <- function(searchConditionsList = NULL, ActionID = F, CreatedTime = F, Description = F, Duration = F, EmailFrom = F, EmailTo = F, ModifiedTime = F, ProjectID = F, RMSActionID = F, RMSEmailID = F, StartTime = F, StopTime = F, Summary = F, Type = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SupportCenter", objectName = "Action", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SupportCenterAction
	#'
	#' This function returns a dataframe or json object of a SupportCenterAction
	#' @param SupportCenterActionID The ID of the SupportCenterAction to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SupportCenterAction. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SupportCenterAction.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SupportCenterAction') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SupportCenter
	#' @return A dataframe or of SupportCenterAction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSupportCenterAction <- function(SupportCenterActionID, ActionID = F, CreatedTime = F, Description = F, Duration = F, EmailFrom = F, EmailTo = F, ModifiedTime = F, ProjectID = F, RMSActionID = F, RMSEmailID = F, StartTime = F, StopTime = F, Summary = F, Type = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SupportCenterActionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SupportCenter", objectName = "Action", objectId = SupportCenterActionID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SupportCenterAction
	#'
	#' This function deletes a SupportCenterAction
	#' @param SupportCenterActionID The ID of the SupportCenterAction to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SupportCenter
	#' @return The SupportCenterActionID of the deleted SupportCenterAction.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSupportCenterAction <- function(SupportCenterActionID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SupportCenter", objectName = "Action", objectId = SupportCenterActionID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SupportCenterAction
	#'
	#' This function creates a SupportCenterAction
	#' @param fieldNames The field values to give the created SupportCenterAction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SupportCenter
	#' @return A newly created SupportCenterAction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSupportCenterAction <- function(Description = NULL, EmailFrom = NULL, EmailTo = NULL, ProjectID = NULL, RMSActionID = NULL, RMSEmailID = NULL, StartTime = NULL, StopTime = NULL, Summary = NULL, Type = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SupportCenter", objectName = "Action", body = list(DataObject = body), searchFields = append("ActionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SupportCenterAction
	#'
	#' This function modifies a SupportCenterAction
	#' @param fieldNames The field values to give the modified SupportCenterAction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SupportCenter
	#' @return The modified SupportCenterAction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySupportCenterAction <- function(ActionID, Description = NULL, EmailFrom = NULL, EmailTo = NULL, ProjectID = NULL, RMSActionID = NULL, RMSEmailID = NULL, StartTime = NULL, StopTime = NULL, Summary = NULL, Type = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SupportCenter", objectName = "Action", objectId = ActionID, body = list(DataObject = body), searchFields = append("ActionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Projects
	#'
	#' This function returns a dataframe or json object of Projects
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Projects. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Projects.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Project') to get more field paths.
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
	#' @concept SupportCenter
	#' @return A list of Projects
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listProjects <- function(searchConditionsList = NULL, Created = F, CreatedTime = F, Critical = F, Description = F, Modified = F, ModifiedTime = F, Origin = F, ProjectID = F, ReportedRelease = F, Resolution = F, RMSProjectID = F, SecurityLocation = F, Status = F, Summary = F, Type = F, TypeDetails = F, UserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SupportCenter", objectName = "Project", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Project
	#'
	#' This function returns a dataframe or json object of a Project
	#' @param ProjectID The ID of the Project to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Project. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Project.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Project') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SupportCenter
	#' @return A dataframe or of Project
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getProject <- function(ProjectID, Created = F, CreatedTime = F, Critical = F, Description = F, Modified = F, ModifiedTime = F, Origin = F, ReportedRelease = F, Resolution = F, RMSProjectID = F, SecurityLocation = F, Status = F, Summary = F, Type = F, TypeDetails = F, UserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ProjectID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SupportCenter", objectName = "Project", objectId = ProjectID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Project
	#'
	#' This function deletes a Project
	#' @param ProjectID The ID of the Project to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SupportCenter
	#' @return The ProjectID of the deleted Project.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteProject <- function(ProjectID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SupportCenter", objectName = "Project", objectId = ProjectID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Project
	#'
	#' This function creates a Project
	#' @param fieldNames The field values to give the created Project. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SupportCenter
	#' @return A newly created Project
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createProject <- function(Created = NULL, Critical = NULL, Description = NULL, Modified = NULL, Origin = NULL, ReportedRelease = NULL, Resolution = NULL, RMSProjectID = NULL, SecurityLocation = NULL, Status = NULL, Summary = NULL, Type = NULL, UserID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SupportCenter", objectName = "Project", body = list(DataObject = body), searchFields = append("ProjectID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Project
	#'
	#' This function modifies a Project
	#' @param fieldNames The field values to give the modified Project. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SupportCenter
	#' @return The modified Project
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyProject <- function(ProjectID, Created = NULL, Critical = NULL, Description = NULL, Modified = NULL, Origin = NULL, ReportedRelease = NULL, Resolution = NULL, RMSProjectID = NULL, SecurityLocation = NULL, Status = NULL, Summary = NULL, Type = NULL, UserID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SupportCenter", objectName = "Project", objectId = ProjectID, body = list(DataObject = body), searchFields = append("ProjectID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List NewFeatureModulePaths
	#'
	#' This function returns a dataframe or json object of NewFeatureModulePaths
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NewFeatureModulePaths. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NewFeatureModulePaths.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NewFeatureModulePath') to get more field paths.
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
	#' @concept SupportCenter
	#' @return A list of NewFeatureModulePaths
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNewFeatureModulePaths <- function(searchConditionsList = NULL, CreatedTime = F, ModifiedTime = F, Module = F, Name = F, NewFeatureID = F, NewFeatureModulePathID = F, Object = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SupportCenter", objectName = "NewFeatureModulePath", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NewFeatureModulePath
	#'
	#' This function returns a dataframe or json object of a NewFeatureModulePath
	#' @param NewFeatureModulePathID The ID of the NewFeatureModulePath to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NewFeatureModulePath. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NewFeatureModulePath.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NewFeatureModulePath') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SupportCenter
	#' @return A dataframe or of NewFeatureModulePath
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNewFeatureModulePath <- function(NewFeatureModulePathID, CreatedTime = F, ModifiedTime = F, Module = F, Name = F, NewFeatureID = F, Object = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NewFeatureModulePathID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SupportCenter", objectName = "NewFeatureModulePath", objectId = NewFeatureModulePathID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NewFeatureModulePath
	#'
	#' This function deletes a NewFeatureModulePath
	#' @param NewFeatureModulePathID The ID of the NewFeatureModulePath to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SupportCenter
	#' @return The NewFeatureModulePathID of the deleted NewFeatureModulePath.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNewFeatureModulePath <- function(NewFeatureModulePathID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SupportCenter", objectName = "NewFeatureModulePath", objectId = NewFeatureModulePathID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NewFeatureModulePath
	#'
	#' This function creates a NewFeatureModulePath
	#' @param fieldNames The field values to give the created NewFeatureModulePath. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SupportCenter
	#' @return A newly created NewFeatureModulePath
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNewFeatureModulePath <- function(Module = NULL, Name = NULL, NewFeatureID = NULL, Object = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SupportCenter", objectName = "NewFeatureModulePath", body = list(DataObject = body), searchFields = append("NewFeatureModulePathID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NewFeatureModulePath
	#'
	#' This function modifies a NewFeatureModulePath
	#' @param fieldNames The field values to give the modified NewFeatureModulePath. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SupportCenter
	#' @return The modified NewFeatureModulePath
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNewFeatureModulePath <- function(NewFeatureModulePathID, Module = NULL, Name = NULL, NewFeatureID = NULL, Object = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SupportCenter", objectName = "NewFeatureModulePath", objectId = NewFeatureModulePathID, body = list(DataObject = body), searchFields = append("NewFeatureModulePathID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List NewFeatureUsers
	#'
	#' This function returns a dataframe or json object of NewFeatureUsers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NewFeatureUsers. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NewFeatureUsers.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NewFeatureUser') to get more field paths.
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
	#' @concept SupportCenter
	#' @return A list of NewFeatureUsers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNewFeatureUsers <- function(searchConditionsList = NULL, CreatedTime = F, MarkedAsRead = F, ModifiedTime = F, NewFeatureID = F, NewFeatureUserID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserIDViewer = F, ViewCount = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SupportCenter", objectName = "NewFeatureUser", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NewFeatureUser
	#'
	#' This function returns a dataframe or json object of a NewFeatureUser
	#' @param NewFeatureUserID The ID of the NewFeatureUser to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NewFeatureUser. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NewFeatureUser.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NewFeatureUser') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SupportCenter
	#' @return A dataframe or of NewFeatureUser
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNewFeatureUser <- function(NewFeatureUserID, CreatedTime = F, MarkedAsRead = F, ModifiedTime = F, NewFeatureID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserIDViewer = F, ViewCount = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NewFeatureUserID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SupportCenter", objectName = "NewFeatureUser", objectId = NewFeatureUserID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NewFeatureUser
	#'
	#' This function deletes a NewFeatureUser
	#' @param NewFeatureUserID The ID of the NewFeatureUser to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SupportCenter
	#' @return The NewFeatureUserID of the deleted NewFeatureUser.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNewFeatureUser <- function(NewFeatureUserID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SupportCenter", objectName = "NewFeatureUser", objectId = NewFeatureUserID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NewFeatureUser
	#'
	#' This function creates a NewFeatureUser
	#' @param fieldNames The field values to give the created NewFeatureUser. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SupportCenter
	#' @return A newly created NewFeatureUser
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNewFeatureUser <- function(MarkedAsRead = NULL, NewFeatureID = NULL, UserIDViewer = NULL, ViewCount = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SupportCenter", objectName = "NewFeatureUser", body = list(DataObject = body), searchFields = append("NewFeatureUserID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NewFeatureUser
	#'
	#' This function modifies a NewFeatureUser
	#' @param fieldNames The field values to give the modified NewFeatureUser. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SupportCenter
	#' @return The modified NewFeatureUser
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNewFeatureUser <- function(NewFeatureUserID, MarkedAsRead = NULL, NewFeatureID = NULL, UserIDViewer = NULL, ViewCount = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SupportCenter", objectName = "NewFeatureUser", objectId = NewFeatureUserID, body = list(DataObject = body), searchFields = append("NewFeatureUserID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List NewFeatures
	#'
	#' This function returns a dataframe or json object of NewFeatures
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NewFeatures. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NewFeatures.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NewFeature') to get more field paths.
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
	#' @concept SupportCenter
	#' @return A list of NewFeatures
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNewFeatures <- function(searchConditionsList = NULL, CreatedTime = F, Description = F, DocumentationURL = F, Duration = F, IsActive = F, IsNew = F, ModifiedTime = F, Modules = F, NewFeatureID = F, PDCURL = F, Portal = F, PRNumber = F, ReleasedVersionBuild = F, ReleasedVersionMajor = F, ReleasedVersionMinor = F, ReleasedVersionRevision = F, ReleaseNotesURL = F, RMSID = F, Summary = F, SystemVersionID = F, Tagline = F, Type = F, UserIDCreatedBy = F, UserIDModifiedBy = F, VideoURL = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SupportCenter", objectName = "NewFeature", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NewFeature
	#'
	#' This function returns a dataframe or json object of a NewFeature
	#' @param NewFeatureID The ID of the NewFeature to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NewFeature. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NewFeature.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NewFeature') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SupportCenter
	#' @return A dataframe or of NewFeature
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNewFeature <- function(NewFeatureID, CreatedTime = F, Description = F, DocumentationURL = F, Duration = F, IsActive = F, IsNew = F, ModifiedTime = F, Modules = F, PDCURL = F, Portal = F, PRNumber = F, ReleasedVersionBuild = F, ReleasedVersionMajor = F, ReleasedVersionMinor = F, ReleasedVersionRevision = F, ReleaseNotesURL = F, RMSID = F, Summary = F, SystemVersionID = F, Tagline = F, Type = F, UserIDCreatedBy = F, UserIDModifiedBy = F, VideoURL = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NewFeatureID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SupportCenter", objectName = "NewFeature", objectId = NewFeatureID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NewFeature
	#'
	#' This function deletes a NewFeature
	#' @param NewFeatureID The ID of the NewFeature to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SupportCenter
	#' @return The NewFeatureID of the deleted NewFeature.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNewFeature <- function(NewFeatureID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SupportCenter", objectName = "NewFeature", objectId = NewFeatureID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NewFeature
	#'
	#' This function creates a NewFeature
	#' @param fieldNames The field values to give the created NewFeature. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SupportCenter
	#' @return A newly created NewFeature
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNewFeature <- function(Description = NULL, DocumentationURL = NULL, Duration = NULL, PDCURL = NULL, Portal = NULL, PRNumber = NULL, ReleasedVersionBuild = NULL, ReleasedVersionMajor = NULL, ReleasedVersionMinor = NULL, ReleasedVersionRevision = NULL, ReleaseNotesURL = NULL, RMSID = NULL, Summary = NULL, SystemVersionID = NULL, Tagline = NULL, Type = NULL, VideoURL = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SupportCenter", objectName = "NewFeature", body = list(DataObject = body), searchFields = append("NewFeatureID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NewFeature
	#'
	#' This function modifies a NewFeature
	#' @param fieldNames The field values to give the modified NewFeature. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SupportCenter
	#' @return The modified NewFeature
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNewFeature <- function(NewFeatureID, Description = NULL, DocumentationURL = NULL, Duration = NULL, PDCURL = NULL, Portal = NULL, PRNumber = NULL, ReleasedVersionBuild = NULL, ReleasedVersionMajor = NULL, ReleasedVersionMinor = NULL, ReleasedVersionRevision = NULL, ReleaseNotesURL = NULL, RMSID = NULL, Summary = NULL, SystemVersionID = NULL, Tagline = NULL, Type = NULL, VideoURL = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SupportCenter", objectName = "NewFeature", objectId = NewFeatureID, body = list(DataObject = body), searchFields = append("NewFeatureID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}
