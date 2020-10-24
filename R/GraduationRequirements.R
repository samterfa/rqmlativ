
	#' List QueuedGraduationPlanRecalcTriggers
	#'
	#' This function returns a dataframe or json object of QueuedGraduationPlanRecalcTriggers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given QueuedGraduationPlanRecalcTriggers. Defaults to FALSE for all return fields which, for convenience, returns all fields for the QueuedGraduationPlanRecalcTriggers.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('QueuedGraduationPlanRecalcTrigger') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of QueuedGraduationPlanRecalcTriggers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listQueuedGraduationPlanRecalcTriggers <- function(searchConditionsList = NULL, QueuedGraduationPlanRecalcTriggerID = F, SourceObjectCode = F, SourceObject = F, SourceID = F, StatusCode = F, Status = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartTime = F, EndTime = F, HostName = F, ThreadName = F, ProcessID = F, UserIDCreatedByImpersonator = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "QueuedGraduationPlanRecalcTrigger", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a QueuedGraduationPlanRecalcTrigger
	#'
	#' This function returns a dataframe or json object of a QueuedGraduationPlanRecalcTrigger
	#' @param QueuedGraduationPlanRecalcTriggerID The ID of the QueuedGraduationPlanRecalcTrigger to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given QueuedGraduationPlanRecalcTrigger. Defaults to FALSE for all return fields which, for convenience, returns all fields for the QueuedGraduationPlanRecalcTrigger.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('QueuedGraduationPlanRecalcTrigger') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of QueuedGraduationPlanRecalcTrigger
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getQueuedGraduationPlanRecalcTrigger <- function(QueuedGraduationPlanRecalcTriggerID, SourceObjectCode = F, SourceObject = F, SourceID = F, StatusCode = F, Status = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartTime = F, EndTime = F, HostName = F, ThreadName = F, ProcessID = F, UserIDCreatedByImpersonator = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "QueuedGraduationPlanRecalcTriggerID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "QueuedGraduationPlanRecalcTrigger", objectId = QueuedGraduationPlanRecalcTriggerID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a QueuedGraduationPlanRecalcTrigger
	#'
	#' This function deletes a QueuedGraduationPlanRecalcTrigger
	#' @param QueuedGraduationPlanRecalcTriggerID The ID of the QueuedGraduationPlanRecalcTrigger to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The QueuedGraduationPlanRecalcTriggerID of the deleted QueuedGraduationPlanRecalcTrigger.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteQueuedGraduationPlanRecalcTrigger <- function(QueuedGraduationPlanRecalcTriggerID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "QueuedGraduationPlanRecalcTrigger", objectId = QueuedGraduationPlanRecalcTriggerID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a QueuedGraduationPlanRecalcTrigger
	#'
	#' This function creates a QueuedGraduationPlanRecalcTrigger
	#' @param fieldNames The field values to give the created QueuedGraduationPlanRecalcTrigger. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created QueuedGraduationPlanRecalcTrigger
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createQueuedGraduationPlanRecalcTrigger <- function(SourceObject = NULL, SourceID = NULL, Status = NULL, StartTime = NULL, EndTime = NULL, HostName = NULL, ThreadName = NULL, ProcessID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "QueuedGraduationPlanRecalcTrigger", body = list(DataObject = body), searchFields = append("QueuedGraduationPlanRecalcTriggerID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a QueuedGraduationPlanRecalcTrigger
	#'
	#' This function modifies a QueuedGraduationPlanRecalcTrigger
	#' @param fieldNames The field values to give the modified QueuedGraduationPlanRecalcTrigger. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified QueuedGraduationPlanRecalcTrigger
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyQueuedGraduationPlanRecalcTrigger <- function(QueuedGraduationPlanRecalcTriggerID, SourceObject = NULL, SourceID = NULL, Status = NULL, StartTime = NULL, EndTime = NULL, HostName = NULL, ThreadName = NULL, ProcessID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "QueuedGraduationPlanRecalcTrigger", objectId = QueuedGraduationPlanRecalcTriggerID, body = list(DataObject = body), searchFields = append("QueuedGraduationPlanRecalcTriggerID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List QueuedStudentPlanCourseworkApplications
	#'
	#' This function returns a dataframe or json object of QueuedStudentPlanCourseworkApplications
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given QueuedStudentPlanCourseworkApplications. Defaults to FALSE for all return fields which, for convenience, returns all fields for the QueuedStudentPlanCourseworkApplications.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('QueuedStudentPlanCourseworkApplication') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of QueuedStudentPlanCourseworkApplications
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listQueuedStudentPlanCourseworkApplications <- function(searchConditionsList = NULL, QueuedStudentPlanCourseworkApplicationID = F, StudentPlanID = F, DistrictID = F, StatusCode = F, Status = F, StartTime = F, EndTime = F, HostName = F, ThreadName = F, ProcessID = F, UserIDCreatedByImpersonator = F, RecalculationStatusDetails = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "QueuedStudentPlanCourseworkApplication", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a QueuedStudentPlanCourseworkApplication
	#'
	#' This function returns a dataframe or json object of a QueuedStudentPlanCourseworkApplication
	#' @param QueuedStudentPlanCourseworkApplicationID The ID of the QueuedStudentPlanCourseworkApplication to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given QueuedStudentPlanCourseworkApplication. Defaults to FALSE for all return fields which, for convenience, returns all fields for the QueuedStudentPlanCourseworkApplication.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('QueuedStudentPlanCourseworkApplication') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of QueuedStudentPlanCourseworkApplication
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getQueuedStudentPlanCourseworkApplication <- function(QueuedStudentPlanCourseworkApplicationID, StudentPlanID = F, DistrictID = F, StatusCode = F, Status = F, StartTime = F, EndTime = F, HostName = F, ThreadName = F, ProcessID = F, UserIDCreatedByImpersonator = F, RecalculationStatusDetails = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "QueuedStudentPlanCourseworkApplicationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "QueuedStudentPlanCourseworkApplication", objectId = QueuedStudentPlanCourseworkApplicationID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a QueuedStudentPlanCourseworkApplication
	#'
	#' This function deletes a QueuedStudentPlanCourseworkApplication
	#' @param QueuedStudentPlanCourseworkApplicationID The ID of the QueuedStudentPlanCourseworkApplication to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The QueuedStudentPlanCourseworkApplicationID of the deleted QueuedStudentPlanCourseworkApplication.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteQueuedStudentPlanCourseworkApplication <- function(QueuedStudentPlanCourseworkApplicationID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "QueuedStudentPlanCourseworkApplication", objectId = QueuedStudentPlanCourseworkApplicationID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a QueuedStudentPlanCourseworkApplication
	#'
	#' This function creates a QueuedStudentPlanCourseworkApplication
	#' @param fieldNames The field values to give the created QueuedStudentPlanCourseworkApplication. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created QueuedStudentPlanCourseworkApplication
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createQueuedStudentPlanCourseworkApplication <- function(StudentPlanID = NULL, DistrictID = NULL, Status = NULL, StartTime = NULL, EndTime = NULL, HostName = NULL, ThreadName = NULL, ProcessID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "QueuedStudentPlanCourseworkApplication", body = list(DataObject = body), searchFields = append("QueuedStudentPlanCourseworkApplicationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a QueuedStudentPlanCourseworkApplication
	#'
	#' This function modifies a QueuedStudentPlanCourseworkApplication
	#' @param fieldNames The field values to give the modified QueuedStudentPlanCourseworkApplication. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified QueuedStudentPlanCourseworkApplication
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyQueuedStudentPlanCourseworkApplication <- function(QueuedStudentPlanCourseworkApplicationID, StudentPlanID = NULL, DistrictID = NULL, Status = NULL, StartTime = NULL, EndTime = NULL, HostName = NULL, ThreadName = NULL, ProcessID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "QueuedStudentPlanCourseworkApplication", objectId = QueuedStudentPlanCourseworkApplicationID, body = list(DataObject = body), searchFields = append("QueuedStudentPlanCourseworkApplicationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempFailedStudentSubAreaWaivers
	#'
	#' This function returns a dataframe or json object of TempFailedStudentSubAreaWaivers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempFailedStudentSubAreaWaivers. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempFailedStudentSubAreaWaivers.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempFailedStudentSubAreaWaiver') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of TempFailedStudentSubAreaWaivers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempFailedStudentSubAreaWaivers <- function(searchConditionsList = NULL, TempFailedStudentSubAreaWaiverID = F, StudentSubAreaID = F, AreaSubAreaDescription = F, Credits = F, Note = F, ActionType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "TempFailedStudentSubAreaWaiver", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempFailedStudentSubAreaWaiver
	#'
	#' This function returns a dataframe or json object of a TempFailedStudentSubAreaWaiver
	#' @param TempFailedStudentSubAreaWaiverID The ID of the TempFailedStudentSubAreaWaiver to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempFailedStudentSubAreaWaiver. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempFailedStudentSubAreaWaiver.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempFailedStudentSubAreaWaiver') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of TempFailedStudentSubAreaWaiver
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempFailedStudentSubAreaWaiver <- function(TempFailedStudentSubAreaWaiverID, StudentSubAreaID = F, AreaSubAreaDescription = F, Credits = F, Note = F, ActionType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempFailedStudentSubAreaWaiverID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "TempFailedStudentSubAreaWaiver", objectId = TempFailedStudentSubAreaWaiverID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempFailedStudentSubAreaWaiver
	#'
	#' This function deletes a TempFailedStudentSubAreaWaiver
	#' @param TempFailedStudentSubAreaWaiverID The ID of the TempFailedStudentSubAreaWaiver to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The TempFailedStudentSubAreaWaiverID of the deleted TempFailedStudentSubAreaWaiver.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempFailedStudentSubAreaWaiver <- function(TempFailedStudentSubAreaWaiverID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "TempFailedStudentSubAreaWaiver", objectId = TempFailedStudentSubAreaWaiverID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempFailedStudentSubAreaWaiver
	#'
	#' This function creates a TempFailedStudentSubAreaWaiver
	#' @param fieldNames The field values to give the created TempFailedStudentSubAreaWaiver. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created TempFailedStudentSubAreaWaiver
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempFailedStudentSubAreaWaiver <- function(StudentSubAreaID = NULL, AreaSubAreaDescription = NULL, Credits = NULL, Note = NULL, ActionType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "TempFailedStudentSubAreaWaiver", body = list(DataObject = body), searchFields = append("TempFailedStudentSubAreaWaiverID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempFailedStudentSubAreaWaiver
	#'
	#' This function modifies a TempFailedStudentSubAreaWaiver
	#' @param fieldNames The field values to give the modified TempFailedStudentSubAreaWaiver. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified TempFailedStudentSubAreaWaiver
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempFailedStudentSubAreaWaiver <- function(TempFailedStudentSubAreaWaiverID, StudentSubAreaID = NULL, AreaSubAreaDescription = NULL, Credits = NULL, Note = NULL, ActionType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "TempFailedStudentSubAreaWaiver", objectId = TempFailedStudentSubAreaWaiverID, body = list(DataObject = body), searchFields = append("TempFailedStudentSubAreaWaiverID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentSubAreas
	#'
	#' This function returns a dataframe or json object of StudentSubAreas
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentSubAreas. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentSubAreas.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentSubArea') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of StudentSubAreas
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentSubAreas <- function(searchConditionsList = NULL, StudentSubAreaID = F, SubAreaID = F, StudentPlanID = F, CanAddWaiver = F, CanHaveWaiver = F, CanAddManualStudentSubAreaCurriculumSubArea = F, TotalManualCredits = F, CompletedCredits = F, InProgressCredits = F, FutureCredits = F, AttemptedCredits = F, RemainingCredits = F, WaivedCredits = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StudentAreaID = F, PlannedCredits = F, IsFulfilledInPlan = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "StudentSubArea", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentSubArea
	#'
	#' This function returns a dataframe or json object of a StudentSubArea
	#' @param StudentSubAreaID The ID of the StudentSubArea to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentSubArea. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentSubArea.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentSubArea') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of StudentSubArea
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentSubArea <- function(StudentSubAreaID, SubAreaID = F, StudentPlanID = F, CanAddWaiver = F, CanHaveWaiver = F, CanAddManualStudentSubAreaCurriculumSubArea = F, TotalManualCredits = F, CompletedCredits = F, InProgressCredits = F, FutureCredits = F, AttemptedCredits = F, RemainingCredits = F, WaivedCredits = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StudentAreaID = F, PlannedCredits = F, IsFulfilledInPlan = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentSubAreaID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "StudentSubArea", objectId = StudentSubAreaID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentSubArea
	#'
	#' This function deletes a StudentSubArea
	#' @param StudentSubAreaID The ID of the StudentSubArea to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The StudentSubAreaID of the deleted StudentSubArea.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentSubArea <- function(StudentSubAreaID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "StudentSubArea", objectId = StudentSubAreaID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentSubArea
	#'
	#' This function creates a StudentSubArea
	#' @param fieldNames The field values to give the created StudentSubArea. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created StudentSubArea
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentSubArea <- function(SubAreaID = NULL, StudentPlanID = NULL, StudentAreaID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "StudentSubArea", body = list(DataObject = body), searchFields = append("StudentSubAreaID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentSubArea
	#'
	#' This function modifies a StudentSubArea
	#' @param fieldNames The field values to give the modified StudentSubArea. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified StudentSubArea
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentSubArea <- function(StudentSubAreaID, SubAreaID = NULL, StudentPlanID = NULL, StudentAreaID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "StudentSubArea", objectId = StudentSubAreaID, body = list(DataObject = body), searchFields = append("StudentSubAreaID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentAreas
	#'
	#' This function returns a dataframe or json object of StudentAreas
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentAreas. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentAreas.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentArea') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of StudentAreas
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentAreas <- function(searchConditionsList = NULL, StudentAreaID = F, StudentPlanID = F, AreaID = F, CompletedCredits = F, InProgressCredits = F, FutureCredits = F, AttemptedCredits = F, RemainingCredits = F, WaivedCredits = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PlannedCredits = F, IsFulfilledInPlan = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "StudentArea", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentArea
	#'
	#' This function returns a dataframe or json object of a StudentArea
	#' @param StudentAreaID The ID of the StudentArea to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentArea. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentArea.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentArea') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of StudentArea
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentArea <- function(StudentAreaID, StudentPlanID = F, AreaID = F, CompletedCredits = F, InProgressCredits = F, FutureCredits = F, AttemptedCredits = F, RemainingCredits = F, WaivedCredits = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PlannedCredits = F, IsFulfilledInPlan = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentAreaID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "StudentArea", objectId = StudentAreaID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentArea
	#'
	#' This function deletes a StudentArea
	#' @param StudentAreaID The ID of the StudentArea to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The StudentAreaID of the deleted StudentArea.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentArea <- function(StudentAreaID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "StudentArea", objectId = StudentAreaID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentArea
	#'
	#' This function creates a StudentArea
	#' @param fieldNames The field values to give the created StudentArea. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created StudentArea
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentArea <- function(StudentPlanID = NULL, AreaID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "StudentArea", body = list(DataObject = body), searchFields = append("StudentAreaID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentArea
	#'
	#' This function modifies a StudentArea
	#' @param fieldNames The field values to give the modified StudentArea. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified StudentArea
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentArea <- function(StudentAreaID, StudentPlanID = NULL, AreaID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "StudentArea", objectId = StudentAreaID, body = list(DataObject = body), searchFields = append("StudentAreaID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentSubAreaWaivers
	#'
	#' This function returns a dataframe or json object of StudentSubAreaWaivers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentSubAreaWaivers. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentSubAreaWaivers.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentSubAreaWaiver') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of StudentSubAreaWaivers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentSubAreaWaivers <- function(searchConditionsList = NULL, StudentSubAreaWaiverID = F, StudentSubAreaID = F, Credits = F, Comment = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "StudentSubAreaWaiver", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentSubAreaWaiver
	#'
	#' This function returns a dataframe or json object of a StudentSubAreaWaiver
	#' @param StudentSubAreaWaiverID The ID of the StudentSubAreaWaiver to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentSubAreaWaiver. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentSubAreaWaiver.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentSubAreaWaiver') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of StudentSubAreaWaiver
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentSubAreaWaiver <- function(StudentSubAreaWaiverID, StudentSubAreaID = F, Credits = F, Comment = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentSubAreaWaiverID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "StudentSubAreaWaiver", objectId = StudentSubAreaWaiverID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentSubAreaWaiver
	#'
	#' This function deletes a StudentSubAreaWaiver
	#' @param StudentSubAreaWaiverID The ID of the StudentSubAreaWaiver to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The StudentSubAreaWaiverID of the deleted StudentSubAreaWaiver.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentSubAreaWaiver <- function(StudentSubAreaWaiverID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "StudentSubAreaWaiver", objectId = StudentSubAreaWaiverID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentSubAreaWaiver
	#'
	#' This function creates a StudentSubAreaWaiver
	#' @param fieldNames The field values to give the created StudentSubAreaWaiver. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created StudentSubAreaWaiver
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentSubAreaWaiver <- function(StudentSubAreaID = NULL, Credits = NULL, Comment = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "StudentSubAreaWaiver", body = list(DataObject = body), searchFields = append("StudentSubAreaWaiverID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentSubAreaWaiver
	#'
	#' This function modifies a StudentSubAreaWaiver
	#' @param fieldNames The field values to give the modified StudentSubAreaWaiver. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified StudentSubAreaWaiver
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentSubAreaWaiver <- function(StudentSubAreaWaiverID, StudentSubAreaID = NULL, Credits = NULL, Comment = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "StudentSubAreaWaiver", objectId = StudentSubAreaWaiverID, body = list(DataObject = body), searchFields = append("StudentSubAreaWaiverID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SubAreaLimitGroups
	#'
	#' This function returns a dataframe or json object of SubAreaLimitGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SubAreaLimitGroups. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SubAreaLimitGroups.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SubAreaLimitGroup') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of SubAreaLimitGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSubAreaLimitGroups <- function(searchConditionsList = NULL, SubAreaLimitGroupID = F, SubAreaID = F, Code = F, Description = F, CreditLimit = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "SubAreaLimitGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SubAreaLimitGroup
	#'
	#' This function returns a dataframe or json object of a SubAreaLimitGroup
	#' @param SubAreaLimitGroupID The ID of the SubAreaLimitGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SubAreaLimitGroup. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SubAreaLimitGroup.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SubAreaLimitGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of SubAreaLimitGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSubAreaLimitGroup <- function(SubAreaLimitGroupID, SubAreaID = F, Code = F, Description = F, CreditLimit = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SubAreaLimitGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "SubAreaLimitGroup", objectId = SubAreaLimitGroupID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SubAreaLimitGroup
	#'
	#' This function deletes a SubAreaLimitGroup
	#' @param SubAreaLimitGroupID The ID of the SubAreaLimitGroup to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The SubAreaLimitGroupID of the deleted SubAreaLimitGroup.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSubAreaLimitGroup <- function(SubAreaLimitGroupID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "SubAreaLimitGroup", objectId = SubAreaLimitGroupID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SubAreaLimitGroup
	#'
	#' This function creates a SubAreaLimitGroup
	#' @param fieldNames The field values to give the created SubAreaLimitGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created SubAreaLimitGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSubAreaLimitGroup <- function(SubAreaID = NULL, Code = NULL, Description = NULL, CreditLimit = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "SubAreaLimitGroup", body = list(DataObject = body), searchFields = append("SubAreaLimitGroupID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SubAreaLimitGroup
	#'
	#' This function modifies a SubAreaLimitGroup
	#' @param fieldNames The field values to give the modified SubAreaLimitGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified SubAreaLimitGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySubAreaLimitGroup <- function(SubAreaLimitGroupID, SubAreaID = NULL, Code = NULL, Description = NULL, CreditLimit = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "SubAreaLimitGroup", objectId = SubAreaLimitGroupID, body = list(DataObject = body), searchFields = append("SubAreaLimitGroupID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SubAreaLimitGroupCurriculumSubAreas
	#'
	#' This function returns a dataframe or json object of SubAreaLimitGroupCurriculumSubAreas
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SubAreaLimitGroupCurriculumSubAreas. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SubAreaLimitGroupCurriculumSubAreas.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SubAreaLimitGroupCurriculumSubArea') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of SubAreaLimitGroupCurriculumSubAreas
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSubAreaLimitGroupCurriculumSubAreas <- function(searchConditionsList = NULL, SubAreaLimitGroupCurriculumSubAreaID = F, SubAreaLimitGroupID = F, CurriculumSubAreaID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "SubAreaLimitGroupCurriculumSubArea", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SubAreaLimitGroupCurriculumSubArea
	#'
	#' This function returns a dataframe or json object of a SubAreaLimitGroupCurriculumSubArea
	#' @param SubAreaLimitGroupCurriculumSubAreaID The ID of the SubAreaLimitGroupCurriculumSubArea to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SubAreaLimitGroupCurriculumSubArea. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SubAreaLimitGroupCurriculumSubArea.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SubAreaLimitGroupCurriculumSubArea') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of SubAreaLimitGroupCurriculumSubArea
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSubAreaLimitGroupCurriculumSubArea <- function(SubAreaLimitGroupCurriculumSubAreaID, SubAreaLimitGroupID = F, CurriculumSubAreaID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SubAreaLimitGroupCurriculumSubAreaID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "SubAreaLimitGroupCurriculumSubArea", objectId = SubAreaLimitGroupCurriculumSubAreaID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SubAreaLimitGroupCurriculumSubArea
	#'
	#' This function deletes a SubAreaLimitGroupCurriculumSubArea
	#' @param SubAreaLimitGroupCurriculumSubAreaID The ID of the SubAreaLimitGroupCurriculumSubArea to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The SubAreaLimitGroupCurriculumSubAreaID of the deleted SubAreaLimitGroupCurriculumSubArea.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSubAreaLimitGroupCurriculumSubArea <- function(SubAreaLimitGroupCurriculumSubAreaID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "SubAreaLimitGroupCurriculumSubArea", objectId = SubAreaLimitGroupCurriculumSubAreaID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SubAreaLimitGroupCurriculumSubArea
	#'
	#' This function creates a SubAreaLimitGroupCurriculumSubArea
	#' @param fieldNames The field values to give the created SubAreaLimitGroupCurriculumSubArea. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created SubAreaLimitGroupCurriculumSubArea
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSubAreaLimitGroupCurriculumSubArea <- function(SubAreaLimitGroupID = NULL, CurriculumSubAreaID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "SubAreaLimitGroupCurriculumSubArea", body = list(DataObject = body), searchFields = append("SubAreaLimitGroupCurriculumSubAreaID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SubAreaLimitGroupCurriculumSubArea
	#'
	#' This function modifies a SubAreaLimitGroupCurriculumSubArea
	#' @param fieldNames The field values to give the modified SubAreaLimitGroupCurriculumSubArea. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified SubAreaLimitGroupCurriculumSubArea
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySubAreaLimitGroupCurriculumSubArea <- function(SubAreaLimitGroupCurriculumSubAreaID, SubAreaLimitGroupID = NULL, CurriculumSubAreaID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "SubAreaLimitGroupCurriculumSubArea", objectId = SubAreaLimitGroupCurriculumSubAreaID, body = list(DataObject = body), searchFields = append("SubAreaLimitGroupCurriculumSubAreaID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempFailedStudentSubAreaCurriculumSubAreas
	#'
	#' This function returns a dataframe or json object of TempFailedStudentSubAreaCurriculumSubAreas
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempFailedStudentSubAreaCurriculumSubAreas. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempFailedStudentSubAreaCurriculumSubAreas.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempFailedStudentSubAreaCurriculumSubArea') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of TempFailedStudentSubAreaCurriculumSubAreas
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempFailedStudentSubAreaCurriculumSubAreas <- function(searchConditionsList = NULL, TempFailedStudentSubAreaCurriculumSubAreaID = F, CurriculumSubAreaID = F, StudentSubAreaID = F, AreaSubAreaDescription = F, StudentCourseRequestID = F, AppliedOrder = F, AttemptedCredits = F, CourseCode = F, CourseDescription = F, CompletedCredits = F, EntityCode = F, FutureCredits = F, InProgressCredits = F, SectionCode = F, SchoolYearDescription = F, Note = F, ActionType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "TempFailedStudentSubAreaCurriculumSubArea", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempFailedStudentSubAreaCurriculumSubArea
	#'
	#' This function returns a dataframe or json object of a TempFailedStudentSubAreaCurriculumSubArea
	#' @param TempFailedStudentSubAreaCurriculumSubAreaID The ID of the TempFailedStudentSubAreaCurriculumSubArea to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempFailedStudentSubAreaCurriculumSubArea. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempFailedStudentSubAreaCurriculumSubArea.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempFailedStudentSubAreaCurriculumSubArea') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of TempFailedStudentSubAreaCurriculumSubArea
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempFailedStudentSubAreaCurriculumSubArea <- function(TempFailedStudentSubAreaCurriculumSubAreaID, CurriculumSubAreaID = F, StudentSubAreaID = F, AreaSubAreaDescription = F, StudentCourseRequestID = F, AppliedOrder = F, AttemptedCredits = F, CourseCode = F, CourseDescription = F, CompletedCredits = F, EntityCode = F, FutureCredits = F, InProgressCredits = F, SectionCode = F, SchoolYearDescription = F, Note = F, ActionType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempFailedStudentSubAreaCurriculumSubAreaID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "TempFailedStudentSubAreaCurriculumSubArea", objectId = TempFailedStudentSubAreaCurriculumSubAreaID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempFailedStudentSubAreaCurriculumSubArea
	#'
	#' This function deletes a TempFailedStudentSubAreaCurriculumSubArea
	#' @param TempFailedStudentSubAreaCurriculumSubAreaID The ID of the TempFailedStudentSubAreaCurriculumSubArea to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The TempFailedStudentSubAreaCurriculumSubAreaID of the deleted TempFailedStudentSubAreaCurriculumSubArea.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempFailedStudentSubAreaCurriculumSubArea <- function(TempFailedStudentSubAreaCurriculumSubAreaID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "TempFailedStudentSubAreaCurriculumSubArea", objectId = TempFailedStudentSubAreaCurriculumSubAreaID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempFailedStudentSubAreaCurriculumSubArea
	#'
	#' This function creates a TempFailedStudentSubAreaCurriculumSubArea
	#' @param fieldNames The field values to give the created TempFailedStudentSubAreaCurriculumSubArea. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created TempFailedStudentSubAreaCurriculumSubArea
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempFailedStudentSubAreaCurriculumSubArea <- function(CurriculumSubAreaID = NULL, StudentSubAreaID = NULL, AreaSubAreaDescription = NULL, StudentCourseRequestID = NULL, AppliedOrder = NULL, AttemptedCredits = NULL, CourseCode = NULL, CourseDescription = NULL, CompletedCredits = NULL, EntityCode = NULL, FutureCredits = NULL, InProgressCredits = NULL, SectionCode = NULL, SchoolYearDescription = NULL, Note = NULL, ActionType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "TempFailedStudentSubAreaCurriculumSubArea", body = list(DataObject = body), searchFields = append("TempFailedStudentSubAreaCurriculumSubAreaID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempFailedStudentSubAreaCurriculumSubArea
	#'
	#' This function modifies a TempFailedStudentSubAreaCurriculumSubArea
	#' @param fieldNames The field values to give the modified TempFailedStudentSubAreaCurriculumSubArea. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified TempFailedStudentSubAreaCurriculumSubArea
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempFailedStudentSubAreaCurriculumSubArea <- function(TempFailedStudentSubAreaCurriculumSubAreaID, CurriculumSubAreaID = NULL, StudentSubAreaID = NULL, AreaSubAreaDescription = NULL, StudentCourseRequestID = NULL, AppliedOrder = NULL, AttemptedCredits = NULL, CourseCode = NULL, CourseDescription = NULL, CompletedCredits = NULL, EntityCode = NULL, FutureCredits = NULL, InProgressCredits = NULL, SectionCode = NULL, SchoolYearDescription = NULL, Note = NULL, ActionType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "TempFailedStudentSubAreaCurriculumSubArea", objectId = TempFailedStudentSubAreaCurriculumSubAreaID, body = list(DataObject = body), searchFields = append("TempFailedStudentSubAreaCurriculumSubAreaID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentSubAreaCurriculumSubAreas
	#'
	#' This function returns a dataframe or json object of StudentSubAreaCurriculumSubAreas
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentSubAreaCurriculumSubAreas. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentSubAreaCurriculumSubAreas.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentSubAreaCurriculumSubArea') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of StudentSubAreaCurriculumSubAreas
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentSubAreaCurriculumSubAreas <- function(searchConditionsList = NULL, StudentSubAreaCurriculumSubAreaID = F, CurriculumSubAreaID = F, StudentSubAreaID = F, StudentCourseRequestID = F, EntryMethod = F, AppliedOrder = F, CompletedCredits = F, InProgressCredits = F, FutureCredits = F, AttemptedCredits = F, TotalNonAttemptedCredits = F, TotalCredits = F, IsAutomatic = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PlannedCredits = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "StudentSubAreaCurriculumSubArea", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentSubAreaCurriculumSubArea
	#'
	#' This function returns a dataframe or json object of a StudentSubAreaCurriculumSubArea
	#' @param StudentSubAreaCurriculumSubAreaID The ID of the StudentSubAreaCurriculumSubArea to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentSubAreaCurriculumSubArea. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentSubAreaCurriculumSubArea.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentSubAreaCurriculumSubArea') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of StudentSubAreaCurriculumSubArea
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentSubAreaCurriculumSubArea <- function(StudentSubAreaCurriculumSubAreaID, CurriculumSubAreaID = F, StudentSubAreaID = F, StudentCourseRequestID = F, EntryMethod = F, AppliedOrder = F, CompletedCredits = F, InProgressCredits = F, FutureCredits = F, AttemptedCredits = F, TotalNonAttemptedCredits = F, TotalCredits = F, IsAutomatic = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PlannedCredits = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentSubAreaCurriculumSubAreaID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "StudentSubAreaCurriculumSubArea", objectId = StudentSubAreaCurriculumSubAreaID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentSubAreaCurriculumSubArea
	#'
	#' This function deletes a StudentSubAreaCurriculumSubArea
	#' @param StudentSubAreaCurriculumSubAreaID The ID of the StudentSubAreaCurriculumSubArea to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The StudentSubAreaCurriculumSubAreaID of the deleted StudentSubAreaCurriculumSubArea.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentSubAreaCurriculumSubArea <- function(StudentSubAreaCurriculumSubAreaID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "StudentSubAreaCurriculumSubArea", objectId = StudentSubAreaCurriculumSubAreaID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentSubAreaCurriculumSubArea
	#'
	#' This function creates a StudentSubAreaCurriculumSubArea
	#' @param fieldNames The field values to give the created StudentSubAreaCurriculumSubArea. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created StudentSubAreaCurriculumSubArea
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentSubAreaCurriculumSubArea <- function(CurriculumSubAreaID = NULL, StudentSubAreaID = NULL, StudentCourseRequestID = NULL, EntryMethod = NULL, AppliedOrder = NULL, CompletedCredits = NULL, InProgressCredits = NULL, FutureCredits = NULL, AttemptedCredits = NULL, PlannedCredits = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "StudentSubAreaCurriculumSubArea", body = list(DataObject = body), searchFields = append("StudentSubAreaCurriculumSubAreaID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentSubAreaCurriculumSubArea
	#'
	#' This function modifies a StudentSubAreaCurriculumSubArea
	#' @param fieldNames The field values to give the modified StudentSubAreaCurriculumSubArea. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified StudentSubAreaCurriculumSubArea
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentSubAreaCurriculumSubArea <- function(StudentSubAreaCurriculumSubAreaID, CurriculumSubAreaID = NULL, StudentSubAreaID = NULL, StudentCourseRequestID = NULL, EntryMethod = NULL, AppliedOrder = NULL, CompletedCredits = NULL, InProgressCredits = NULL, FutureCredits = NULL, AttemptedCredits = NULL, PlannedCredits = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "StudentSubAreaCurriculumSubArea", objectId = StudentSubAreaCurriculumSubAreaID, body = list(DataObject = body), searchFields = append("StudentSubAreaCurriculumSubAreaID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PlanDefaults
	#'
	#' This function returns a dataframe or json object of PlanDefaults
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PlanDefaults. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PlanDefaults.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PlanDefault') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of PlanDefaults
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPlanDefaults <- function(searchConditionsList = NULL, PlanDefaultID = F, PlanID = F, EntityID = F, GradYearLow = F, GradYearHigh = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "PlanDefault", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PlanDefault
	#'
	#' This function returns a dataframe or json object of a PlanDefault
	#' @param PlanDefaultID The ID of the PlanDefault to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PlanDefault. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PlanDefault.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PlanDefault') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of PlanDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPlanDefault <- function(PlanDefaultID, PlanID = F, EntityID = F, GradYearLow = F, GradYearHigh = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PlanDefaultID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "PlanDefault", objectId = PlanDefaultID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PlanDefault
	#'
	#' This function deletes a PlanDefault
	#' @param PlanDefaultID The ID of the PlanDefault to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The PlanDefaultID of the deleted PlanDefault.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePlanDefault <- function(PlanDefaultID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "PlanDefault", objectId = PlanDefaultID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PlanDefault
	#'
	#' This function creates a PlanDefault
	#' @param fieldNames The field values to give the created PlanDefault. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created PlanDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPlanDefault <- function(PlanID = NULL, EntityID = NULL, GradYearLow = NULL, GradYearHigh = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "PlanDefault", body = list(DataObject = body), searchFields = append("PlanDefaultID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PlanDefault
	#'
	#' This function modifies a PlanDefault
	#' @param fieldNames The field values to give the modified PlanDefault. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified PlanDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPlanDefault <- function(PlanDefaultID, PlanID = NULL, EntityID = NULL, GradYearLow = NULL, GradYearHigh = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "PlanDefault", objectId = PlanDefaultID, body = list(DataObject = body), searchFields = append("PlanDefaultID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List GraduationRequirementsConfigDistricts
	#'
	#' This function returns a dataframe or json object of GraduationRequirementsConfigDistricts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given GraduationRequirementsConfigDistricts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the GraduationRequirementsConfigDistricts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('GraduationRequirementsConfigDistrict') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of GraduationRequirementsConfigDistricts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listGraduationRequirementsConfigDistricts <- function(searchConditionsList = NULL, ConfigDistrictID = F, DistrictID = F, CourseWorkAppliedByType = F, IncludeInProgressCredit = F, IncludeFutureCredit = F, TurnOffAutomaticCalculation = F, GradingPeriodEndDateLastCheckedDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, UsePriorToLastGradeLevel = F, TurnOffAutomaticEndorsementCalculation = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "ConfigDistrict", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a GraduationRequirementsConfigDistrict
	#'
	#' This function returns a dataframe or json object of a GraduationRequirementsConfigDistrict
	#' @param GraduationRequirementsConfigDistrictID The ID of the GraduationRequirementsConfigDistrict to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given GraduationRequirementsConfigDistrict. Defaults to FALSE for all return fields which, for convenience, returns all fields for the GraduationRequirementsConfigDistrict.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('GraduationRequirementsConfigDistrict') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of GraduationRequirementsConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getGraduationRequirementsConfigDistrict <- function(GraduationRequirementsConfigDistrictID, ConfigDistrictID = F, DistrictID = F, CourseWorkAppliedByType = F, IncludeInProgressCredit = F, IncludeFutureCredit = F, TurnOffAutomaticCalculation = F, GradingPeriodEndDateLastCheckedDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, UsePriorToLastGradeLevel = F, TurnOffAutomaticEndorsementCalculation = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "GraduationRequirementsConfigDistrictID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "ConfigDistrict", objectId = GraduationRequirementsConfigDistrictID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a GraduationRequirementsConfigDistrict
	#'
	#' This function deletes a GraduationRequirementsConfigDistrict
	#' @param GraduationRequirementsConfigDistrictID The ID of the GraduationRequirementsConfigDistrict to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The GraduationRequirementsConfigDistrictID of the deleted GraduationRequirementsConfigDistrict.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteGraduationRequirementsConfigDistrict <- function(GraduationRequirementsConfigDistrictID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "ConfigDistrict", objectId = GraduationRequirementsConfigDistrictID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a GraduationRequirementsConfigDistrict
	#'
	#' This function creates a GraduationRequirementsConfigDistrict
	#' @param fieldNames The field values to give the created GraduationRequirementsConfigDistrict. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created GraduationRequirementsConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createGraduationRequirementsConfigDistrict <- function(DistrictID = NULL, CourseWorkAppliedByType = NULL, IncludeInProgressCredit = NULL, IncludeFutureCredit = NULL, TurnOffAutomaticCalculation = NULL, GradingPeriodEndDateLastCheckedDate = NULL, UsePriorToLastGradeLevel = NULL, TurnOffAutomaticEndorsementCalculation = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "ConfigDistrict", body = list(DataObject = body), searchFields = append("ConfigDistrictID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a GraduationRequirementsConfigDistrict
	#'
	#' This function modifies a GraduationRequirementsConfigDistrict
	#' @param fieldNames The field values to give the modified GraduationRequirementsConfigDistrict. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified GraduationRequirementsConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyGraduationRequirementsConfigDistrict <- function(ConfigDistrictID, DistrictID = NULL, CourseWorkAppliedByType = NULL, IncludeInProgressCredit = NULL, IncludeFutureCredit = NULL, TurnOffAutomaticCalculation = NULL, GradingPeriodEndDateLastCheckedDate = NULL, UsePriorToLastGradeLevel = NULL, TurnOffAutomaticEndorsementCalculation = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "ConfigDistrict", objectId = ConfigDistrictID, body = list(DataObject = body), searchFields = append("ConfigDistrictID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentPlans
	#'
	#' This function returns a dataframe or json object of StudentPlans
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentPlans. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentPlans.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentPlan') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of StudentPlans
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentPlans <- function(searchConditionsList = NULL, StudentPlanID = F, StudentID = F, PlanID = F, IsCurrent = F, CompletedCredits = F, InProgressCredits = F, FutureCredits = F, AttemptedCredits = F, RemainingCredits = F, WaivedCredits = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PlannedCredits = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "StudentPlan", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentPlan
	#'
	#' This function returns a dataframe or json object of a StudentPlan
	#' @param StudentPlanID The ID of the StudentPlan to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentPlan. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentPlan.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentPlan') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of StudentPlan
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentPlan <- function(StudentPlanID, StudentID = F, PlanID = F, IsCurrent = F, CompletedCredits = F, InProgressCredits = F, FutureCredits = F, AttemptedCredits = F, RemainingCredits = F, WaivedCredits = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PlannedCredits = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentPlanID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "StudentPlan", objectId = StudentPlanID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentPlan
	#'
	#' This function deletes a StudentPlan
	#' @param StudentPlanID The ID of the StudentPlan to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The StudentPlanID of the deleted StudentPlan.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentPlan <- function(StudentPlanID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "StudentPlan", objectId = StudentPlanID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentPlan
	#'
	#' This function creates a StudentPlan
	#' @param fieldNames The field values to give the created StudentPlan. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created StudentPlan
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentPlan <- function(StudentID = NULL, PlanID = NULL, IsCurrent = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "StudentPlan", body = list(DataObject = body), searchFields = append("StudentPlanID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentPlan
	#'
	#' This function modifies a StudentPlan
	#' @param fieldNames The field values to give the modified StudentPlan. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified StudentPlan
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentPlan <- function(StudentPlanID, StudentID = NULL, PlanID = NULL, IsCurrent = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "StudentPlan", objectId = StudentPlanID, body = list(DataObject = body), searchFields = append("StudentPlanID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Areas
	#'
	#' This function returns a dataframe or json object of Areas
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Areas. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Areas.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Area') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of Areas
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAreas <- function(searchConditionsList = NULL, AreaID = F, PlanID = F, Description = F, TotalCredits = F, DisplayOrder = F, IsElective = F, SkywardID = F, ElectiveSubAreaID = F, IsNotElective = F, IsSystemArea = F, IsNotSystemArea = F, NonElectiveCreditTotal = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, GradReqRankGPARequiredCourseRuleOverride = F, UseGradReqSubjectType = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "Area", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an Area
	#'
	#' This function returns a dataframe or json object of an Area
	#' @param AreaID The ID of the Area to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Area. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Area.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Area') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of Area
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getArea <- function(AreaID, PlanID = F, Description = F, TotalCredits = F, DisplayOrder = F, IsElective = F, SkywardID = F, ElectiveSubAreaID = F, IsNotElective = F, IsSystemArea = F, IsNotSystemArea = F, NonElectiveCreditTotal = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, GradReqRankGPARequiredCourseRuleOverride = F, UseGradReqSubjectType = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AreaID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "Area", objectId = AreaID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an Area
	#'
	#' This function deletes an Area
	#' @param AreaID The ID of the Area to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The AreaID of the deleted Area.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteArea <- function(AreaID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "Area", objectId = AreaID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an Area
	#'
	#' This function creates an Area
	#' @param fieldNames The field values to give the created Area. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created Area
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createArea <- function(PlanID = NULL, Description = NULL, TotalCredits = NULL, DisplayOrder = NULL, IsElective = NULL, GradReqRankGPARequiredCourseRuleOverride = NULL, UseGradReqSubjectType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "Area", body = list(DataObject = body), searchFields = append("AreaID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an Area
	#'
	#' This function modifies an Area
	#' @param fieldNames The field values to give the modified Area. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified Area
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyArea <- function(AreaID, PlanID = NULL, Description = NULL, TotalCredits = NULL, DisplayOrder = NULL, IsElective = NULL, GradReqRankGPARequiredCourseRuleOverride = NULL, UseGradReqSubjectType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "Area", objectId = AreaID, body = list(DataObject = body), searchFields = append("AreaID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CurriculumSubAreas
	#'
	#' This function returns a dataframe or json object of CurriculumSubAreas
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CurriculumSubAreas. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CurriculumSubAreas.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CurriculumSubArea') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of CurriculumSubAreas
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCurriculumSubAreas <- function(searchConditionsList = NULL, CurriculumSubAreaID = F, CurriculumID = F, SubAreaID = F, StudentID = F, SchoolYearLow = F, SchoolYearHigh = F, ApplicationOrder = F, MaximumPercentOfCourseCredit = F, AllowReuseOfPreviouslyAppliedCredits = F, IsCustomCurriculumSubAreaWithStudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsGradReqRankGPAWaiver = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "CurriculumSubArea", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CurriculumSubArea
	#'
	#' This function returns a dataframe or json object of a CurriculumSubArea
	#' @param CurriculumSubAreaID The ID of the CurriculumSubArea to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CurriculumSubArea. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CurriculumSubArea.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CurriculumSubArea') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of CurriculumSubArea
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCurriculumSubArea <- function(CurriculumSubAreaID, CurriculumID = F, SubAreaID = F, StudentID = F, SchoolYearLow = F, SchoolYearHigh = F, ApplicationOrder = F, MaximumPercentOfCourseCredit = F, AllowReuseOfPreviouslyAppliedCredits = F, IsCustomCurriculumSubAreaWithStudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsGradReqRankGPAWaiver = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CurriculumSubAreaID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "CurriculumSubArea", objectId = CurriculumSubAreaID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CurriculumSubArea
	#'
	#' This function deletes a CurriculumSubArea
	#' @param CurriculumSubAreaID The ID of the CurriculumSubArea to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The CurriculumSubAreaID of the deleted CurriculumSubArea.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCurriculumSubArea <- function(CurriculumSubAreaID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "CurriculumSubArea", objectId = CurriculumSubAreaID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CurriculumSubArea
	#'
	#' This function creates a CurriculumSubArea
	#' @param fieldNames The field values to give the created CurriculumSubArea. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created CurriculumSubArea
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCurriculumSubArea <- function(CurriculumID = NULL, SubAreaID = NULL, StudentID = NULL, SchoolYearLow = NULL, SchoolYearHigh = NULL, ApplicationOrder = NULL, MaximumPercentOfCourseCredit = NULL, AllowReuseOfPreviouslyAppliedCredits = NULL, IsGradReqRankGPAWaiver = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "CurriculumSubArea", body = list(DataObject = body), searchFields = append("CurriculumSubAreaID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CurriculumSubArea
	#'
	#' This function modifies a CurriculumSubArea
	#' @param fieldNames The field values to give the modified CurriculumSubArea. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified CurriculumSubArea
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCurriculumSubArea <- function(CurriculumSubAreaID, CurriculumID = NULL, SubAreaID = NULL, StudentID = NULL, SchoolYearLow = NULL, SchoolYearHigh = NULL, ApplicationOrder = NULL, MaximumPercentOfCourseCredit = NULL, AllowReuseOfPreviouslyAppliedCredits = NULL, IsGradReqRankGPAWaiver = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "CurriculumSubArea", objectId = CurriculumSubAreaID, body = list(DataObject = body), searchFields = append("CurriculumSubAreaID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PlanEntities
	#'
	#' This function returns a dataframe or json object of PlanEntities
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PlanEntities. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PlanEntities.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PlanEntity') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of PlanEntities
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPlanEntities <- function(searchConditionsList = NULL, PlanEntityID = F, PlanID = F, EntityID = F, GradYearLow = F, GradYearHigh = F, GradYearRange = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "PlanEntity", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PlanEntity
	#'
	#' This function returns a dataframe or json object of a PlanEntity
	#' @param PlanEntityID The ID of the PlanEntity to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PlanEntity. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PlanEntity.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PlanEntity') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of PlanEntity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPlanEntity <- function(PlanEntityID, PlanID = F, EntityID = F, GradYearLow = F, GradYearHigh = F, GradYearRange = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PlanEntityID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "PlanEntity", objectId = PlanEntityID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PlanEntity
	#'
	#' This function deletes a PlanEntity
	#' @param PlanEntityID The ID of the PlanEntity to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The PlanEntityID of the deleted PlanEntity.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePlanEntity <- function(PlanEntityID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "PlanEntity", objectId = PlanEntityID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PlanEntity
	#'
	#' This function creates a PlanEntity
	#' @param fieldNames The field values to give the created PlanEntity. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created PlanEntity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPlanEntity <- function(PlanID = NULL, EntityID = NULL, GradYearLow = NULL, GradYearHigh = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "PlanEntity", body = list(DataObject = body), searchFields = append("PlanEntityID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PlanEntity
	#'
	#' This function modifies a PlanEntity
	#' @param fieldNames The field values to give the modified PlanEntity. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified PlanEntity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPlanEntity <- function(PlanEntityID, PlanID = NULL, EntityID = NULL, GradYearLow = NULL, GradYearHigh = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "PlanEntity", objectId = PlanEntityID, body = list(DataObject = body), searchFields = append("PlanEntityID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Plans
	#'
	#' This function returns a dataframe or json object of Plans
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Plans. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Plans.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Plan') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of Plans
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPlans <- function(searchConditionsList = NULL, PlanID = F, Description = F, GradYearLow = F, GradYearHigh = F, TotalCredits = F, TotalYears = F, SkywardID = F, DistrictID = F, EdFiGraduationPlanID = F, GeneralElectiveSubAreaID = F, IsSystemPlan = F, IsNotSystemPlan = F, NumberOfSubAreasForCurriculum = F, NonElectiveCreditTotal = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, EarnedCreditsMethodIDDefaultOverride = F, EdFiGraduationPlanTypeDescriptorID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "Plan", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Plan
	#'
	#' This function returns a dataframe or json object of a Plan
	#' @param PlanID The ID of the Plan to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Plan. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Plan.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Plan') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of Plan
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPlan <- function(PlanID, Description = F, GradYearLow = F, GradYearHigh = F, TotalCredits = F, TotalYears = F, SkywardID = F, DistrictID = F, EdFiGraduationPlanID = F, GeneralElectiveSubAreaID = F, IsSystemPlan = F, IsNotSystemPlan = F, NumberOfSubAreasForCurriculum = F, NonElectiveCreditTotal = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, EarnedCreditsMethodIDDefaultOverride = F, EdFiGraduationPlanTypeDescriptorID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PlanID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "Plan", objectId = PlanID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Plan
	#'
	#' This function deletes a Plan
	#' @param PlanID The ID of the Plan to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The PlanID of the deleted Plan.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePlan <- function(PlanID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "Plan", objectId = PlanID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Plan
	#'
	#' This function creates a Plan
	#' @param fieldNames The field values to give the created Plan. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created Plan
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPlan <- function(Description = NULL, GradYearLow = NULL, GradYearHigh = NULL, TotalCredits = NULL, DistrictID = NULL, EdFiGraduationPlanID = NULL, EarnedCreditsMethodIDDefaultOverride = NULL, EdFiGraduationPlanTypeDescriptorID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "Plan", body = list(DataObject = body), searchFields = append("PlanID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Plan
	#'
	#' This function modifies a Plan
	#' @param fieldNames The field values to give the modified Plan. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified Plan
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPlan <- function(PlanID, Description = NULL, GradYearLow = NULL, GradYearHigh = NULL, TotalCredits = NULL, DistrictID = NULL, EdFiGraduationPlanID = NULL, EarnedCreditsMethodIDDefaultOverride = NULL, EdFiGraduationPlanTypeDescriptorID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "Plan", objectId = PlanID, body = list(DataObject = body), searchFields = append("PlanID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SubAreas
	#'
	#' This function returns a dataframe or json object of SubAreas
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SubAreas. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SubAreas.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SubArea') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of SubAreas
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSubAreas <- function(searchConditionsList = NULL, SubAreaID = F, AreaID = F, Description = F, Credits = F, DisplayOrder = F, IsElective = F, SkywardID = F, IsSystemSubArea = F, AreaSubAreaDescription = F, HasSkywardID = F, CurriculumSubAreaExistsForNonStudentCurriculum = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, GradReqRankGPARequiredCourseRuleOverride = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "SubArea", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SubArea
	#'
	#' This function returns a dataframe or json object of a SubArea
	#' @param SubAreaID The ID of the SubArea to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SubArea. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SubArea.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SubArea') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of SubArea
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSubArea <- function(SubAreaID, AreaID = F, Description = F, Credits = F, DisplayOrder = F, IsElective = F, SkywardID = F, IsSystemSubArea = F, AreaSubAreaDescription = F, HasSkywardID = F, CurriculumSubAreaExistsForNonStudentCurriculum = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, GradReqRankGPARequiredCourseRuleOverride = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SubAreaID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "SubArea", objectId = SubAreaID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SubArea
	#'
	#' This function deletes a SubArea
	#' @param SubAreaID The ID of the SubArea to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The SubAreaID of the deleted SubArea.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSubArea <- function(SubAreaID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "SubArea", objectId = SubAreaID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SubArea
	#'
	#' This function creates a SubArea
	#' @param fieldNames The field values to give the created SubArea. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created SubArea
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSubArea <- function(AreaID = NULL, Description = NULL, Credits = NULL, DisplayOrder = NULL, IsElective = NULL, GradReqRankGPARequiredCourseRuleOverride = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "SubArea", body = list(DataObject = body), searchFields = append("SubAreaID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SubArea
	#'
	#' This function modifies a SubArea
	#' @param fieldNames The field values to give the modified SubArea. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified SubArea
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySubArea <- function(SubAreaID, AreaID = NULL, Description = NULL, Credits = NULL, DisplayOrder = NULL, IsElective = NULL, GradReqRankGPARequiredCourseRuleOverride = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "SubArea", objectId = SubAreaID, body = list(DataObject = body), searchFields = append("SubAreaID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Endorsements
	#'
	#' This function returns a dataframe or json object of Endorsements
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Endorsements. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Endorsements.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Endorsement') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of Endorsements
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEndorsements <- function(searchConditionsList = NULL, EndorsementID = F, Code = F, Description = F, DistrictID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsActive = F, EndorsementDefaultID = F, IsPreviouslyLoaded = F, IsDeclarable = F, HasEndorsementOptions = F, PrintOnTranscript = F, IsDistrictDefined = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "Endorsement", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an Endorsement
	#'
	#' This function returns a dataframe or json object of an Endorsement
	#' @param EndorsementID The ID of the Endorsement to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Endorsement. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Endorsement.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Endorsement') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of Endorsement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEndorsement <- function(EndorsementID, Code = F, Description = F, DistrictID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsActive = F, EndorsementDefaultID = F, IsPreviouslyLoaded = F, IsDeclarable = F, HasEndorsementOptions = F, PrintOnTranscript = F, IsDistrictDefined = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EndorsementID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "Endorsement", objectId = EndorsementID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an Endorsement
	#'
	#' This function deletes an Endorsement
	#' @param EndorsementID The ID of the Endorsement to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The EndorsementID of the deleted Endorsement.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEndorsement <- function(EndorsementID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "Endorsement", objectId = EndorsementID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an Endorsement
	#'
	#' This function creates an Endorsement
	#' @param fieldNames The field values to give the created Endorsement. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created Endorsement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEndorsement <- function(Code = NULL, Description = NULL, DistrictID = NULL, IsActive = NULL, EndorsementDefaultID = NULL, IsPreviouslyLoaded = NULL, IsDeclarable = NULL, PrintOnTranscript = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "Endorsement", body = list(DataObject = body), searchFields = append("EndorsementID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an Endorsement
	#'
	#' This function modifies an Endorsement
	#' @param fieldNames The field values to give the modified Endorsement. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified Endorsement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEndorsement <- function(EndorsementID, Code = NULL, Description = NULL, DistrictID = NULL, IsActive = NULL, EndorsementDefaultID = NULL, IsPreviouslyLoaded = NULL, IsDeclarable = NULL, PrintOnTranscript = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "Endorsement", objectId = EndorsementID, body = list(DataObject = body), searchFields = append("EndorsementID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EndorsementDeclarationTimePeriodStudentEntityYears
	#'
	#' This function returns a dataframe or json object of EndorsementDeclarationTimePeriodStudentEntityYears
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EndorsementDeclarationTimePeriodStudentEntityYears. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EndorsementDeclarationTimePeriodStudentEntityYears.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EndorsementDeclarationTimePeriodStudentEntityYear') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of EndorsementDeclarationTimePeriodStudentEntityYears
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEndorsementDeclarationTimePeriodStudentEntityYears <- function(searchConditionsList = NULL, EndorsementDeclarationTimePeriodStudentEntityYearID = F, EndorsementDeclarationTimePeriodID = F, StudentEntityYearID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "EndorsementDeclarationTimePeriodStudentEntityYear", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EndorsementDeclarationTimePeriodStudentEntityYear
	#'
	#' This function returns a dataframe or json object of an EndorsementDeclarationTimePeriodStudentEntityYear
	#' @param EndorsementDeclarationTimePeriodStudentEntityYearID The ID of the EndorsementDeclarationTimePeriodStudentEntityYear to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EndorsementDeclarationTimePeriodStudentEntityYear. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EndorsementDeclarationTimePeriodStudentEntityYear.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EndorsementDeclarationTimePeriodStudentEntityYear') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of EndorsementDeclarationTimePeriodStudentEntityYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEndorsementDeclarationTimePeriodStudentEntityYear <- function(EndorsementDeclarationTimePeriodStudentEntityYearID, EndorsementDeclarationTimePeriodID = F, StudentEntityYearID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EndorsementDeclarationTimePeriodStudentEntityYearID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "EndorsementDeclarationTimePeriodStudentEntityYear", objectId = EndorsementDeclarationTimePeriodStudentEntityYearID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EndorsementDeclarationTimePeriodStudentEntityYear
	#'
	#' This function deletes an EndorsementDeclarationTimePeriodStudentEntityYear
	#' @param EndorsementDeclarationTimePeriodStudentEntityYearID The ID of the EndorsementDeclarationTimePeriodStudentEntityYear to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The EndorsementDeclarationTimePeriodStudentEntityYearID of the deleted EndorsementDeclarationTimePeriodStudentEntityYear.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEndorsementDeclarationTimePeriodStudentEntityYear <- function(EndorsementDeclarationTimePeriodStudentEntityYearID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "EndorsementDeclarationTimePeriodStudentEntityYear", objectId = EndorsementDeclarationTimePeriodStudentEntityYearID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EndorsementDeclarationTimePeriodStudentEntityYear
	#'
	#' This function creates an EndorsementDeclarationTimePeriodStudentEntityYear
	#' @param fieldNames The field values to give the created EndorsementDeclarationTimePeriodStudentEntityYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created EndorsementDeclarationTimePeriodStudentEntityYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEndorsementDeclarationTimePeriodStudentEntityYear <- function(EndorsementDeclarationTimePeriodID = NULL, StudentEntityYearID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "EndorsementDeclarationTimePeriodStudentEntityYear", body = list(DataObject = body), searchFields = append("EndorsementDeclarationTimePeriodStudentEntityYearID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EndorsementDeclarationTimePeriodStudentEntityYear
	#'
	#' This function modifies an EndorsementDeclarationTimePeriodStudentEntityYear
	#' @param fieldNames The field values to give the modified EndorsementDeclarationTimePeriodStudentEntityYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified EndorsementDeclarationTimePeriodStudentEntityYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEndorsementDeclarationTimePeriodStudentEntityYear <- function(EndorsementDeclarationTimePeriodStudentEntityYearID, EndorsementDeclarationTimePeriodID = NULL, StudentEntityYearID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "EndorsementDeclarationTimePeriodStudentEntityYear", objectId = EndorsementDeclarationTimePeriodStudentEntityYearID, body = list(DataObject = body), searchFields = append("EndorsementDeclarationTimePeriodStudentEntityYearID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EndorsementDeclarationTimePeriodGradeReferences
	#'
	#' This function returns a dataframe or json object of EndorsementDeclarationTimePeriodGradeReferences
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EndorsementDeclarationTimePeriodGradeReferences. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EndorsementDeclarationTimePeriodGradeReferences.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EndorsementDeclarationTimePeriodGradeReference') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of EndorsementDeclarationTimePeriodGradeReferences
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEndorsementDeclarationTimePeriodGradeReferences <- function(searchConditionsList = NULL, EndorsementDeclarationTimePeriodGradeReferenceID = F, EndorsementDeclarationTimePeriodID = F, GradeReferenceID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "EndorsementDeclarationTimePeriodGradeReference", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EndorsementDeclarationTimePeriodGradeReference
	#'
	#' This function returns a dataframe or json object of an EndorsementDeclarationTimePeriodGradeReference
	#' @param EndorsementDeclarationTimePeriodGradeReferenceID The ID of the EndorsementDeclarationTimePeriodGradeReference to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EndorsementDeclarationTimePeriodGradeReference. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EndorsementDeclarationTimePeriodGradeReference.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EndorsementDeclarationTimePeriodGradeReference') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of EndorsementDeclarationTimePeriodGradeReference
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEndorsementDeclarationTimePeriodGradeReference <- function(EndorsementDeclarationTimePeriodGradeReferenceID, EndorsementDeclarationTimePeriodID = F, GradeReferenceID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EndorsementDeclarationTimePeriodGradeReferenceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "EndorsementDeclarationTimePeriodGradeReference", objectId = EndorsementDeclarationTimePeriodGradeReferenceID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EndorsementDeclarationTimePeriodGradeReference
	#'
	#' This function deletes an EndorsementDeclarationTimePeriodGradeReference
	#' @param EndorsementDeclarationTimePeriodGradeReferenceID The ID of the EndorsementDeclarationTimePeriodGradeReference to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The EndorsementDeclarationTimePeriodGradeReferenceID of the deleted EndorsementDeclarationTimePeriodGradeReference.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEndorsementDeclarationTimePeriodGradeReference <- function(EndorsementDeclarationTimePeriodGradeReferenceID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "EndorsementDeclarationTimePeriodGradeReference", objectId = EndorsementDeclarationTimePeriodGradeReferenceID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EndorsementDeclarationTimePeriodGradeReference
	#'
	#' This function creates an EndorsementDeclarationTimePeriodGradeReference
	#' @param fieldNames The field values to give the created EndorsementDeclarationTimePeriodGradeReference. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created EndorsementDeclarationTimePeriodGradeReference
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEndorsementDeclarationTimePeriodGradeReference <- function(EndorsementDeclarationTimePeriodID = NULL, GradeReferenceID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "EndorsementDeclarationTimePeriodGradeReference", body = list(DataObject = body), searchFields = append("EndorsementDeclarationTimePeriodGradeReferenceID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EndorsementDeclarationTimePeriodGradeReference
	#'
	#' This function modifies an EndorsementDeclarationTimePeriodGradeReference
	#' @param fieldNames The field values to give the modified EndorsementDeclarationTimePeriodGradeReference. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified EndorsementDeclarationTimePeriodGradeReference
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEndorsementDeclarationTimePeriodGradeReference <- function(EndorsementDeclarationTimePeriodGradeReferenceID, EndorsementDeclarationTimePeriodID = NULL, GradeReferenceID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "EndorsementDeclarationTimePeriodGradeReference", objectId = EndorsementDeclarationTimePeriodGradeReferenceID, body = list(DataObject = body), searchFields = append("EndorsementDeclarationTimePeriodGradeReferenceID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EndorsementDeclarationTimePeriods
	#'
	#' This function returns a dataframe or json object of EndorsementDeclarationTimePeriods
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EndorsementDeclarationTimePeriods. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EndorsementDeclarationTimePeriods.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EndorsementDeclarationTimePeriod') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of EndorsementDeclarationTimePeriods
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEndorsementDeclarationTimePeriods <- function(searchConditionsList = NULL, EndorsementDeclarationTimePeriodID = F, EntityID = F, SchoolYearID = F, StartTime = F, EndTime = F, FilterOption = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "EndorsementDeclarationTimePeriod", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EndorsementDeclarationTimePeriod
	#'
	#' This function returns a dataframe or json object of an EndorsementDeclarationTimePeriod
	#' @param EndorsementDeclarationTimePeriodID The ID of the EndorsementDeclarationTimePeriod to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EndorsementDeclarationTimePeriod. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EndorsementDeclarationTimePeriod.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EndorsementDeclarationTimePeriod') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of EndorsementDeclarationTimePeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEndorsementDeclarationTimePeriod <- function(EndorsementDeclarationTimePeriodID, EntityID = F, SchoolYearID = F, StartTime = F, EndTime = F, FilterOption = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EndorsementDeclarationTimePeriodID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "EndorsementDeclarationTimePeriod", objectId = EndorsementDeclarationTimePeriodID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EndorsementDeclarationTimePeriod
	#'
	#' This function deletes an EndorsementDeclarationTimePeriod
	#' @param EndorsementDeclarationTimePeriodID The ID of the EndorsementDeclarationTimePeriod to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The EndorsementDeclarationTimePeriodID of the deleted EndorsementDeclarationTimePeriod.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEndorsementDeclarationTimePeriod <- function(EndorsementDeclarationTimePeriodID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "EndorsementDeclarationTimePeriod", objectId = EndorsementDeclarationTimePeriodID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EndorsementDeclarationTimePeriod
	#'
	#' This function creates an EndorsementDeclarationTimePeriod
	#' @param fieldNames The field values to give the created EndorsementDeclarationTimePeriod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created EndorsementDeclarationTimePeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEndorsementDeclarationTimePeriod <- function(EntityID = NULL, SchoolYearID = NULL, StartTime = NULL, EndTime = NULL, FilterOption = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "EndorsementDeclarationTimePeriod", body = list(DataObject = body), searchFields = append("EndorsementDeclarationTimePeriodID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EndorsementDeclarationTimePeriod
	#'
	#' This function modifies an EndorsementDeclarationTimePeriod
	#' @param fieldNames The field values to give the modified EndorsementDeclarationTimePeriod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified EndorsementDeclarationTimePeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEndorsementDeclarationTimePeriod <- function(EndorsementDeclarationTimePeriodID, EntityID = NULL, SchoolYearID = NULL, StartTime = NULL, EndTime = NULL, FilterOption = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "EndorsementDeclarationTimePeriod", objectId = EndorsementDeclarationTimePeriodID, body = list(DataObject = body), searchFields = append("EndorsementDeclarationTimePeriodID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentEndorsements
	#'
	#' This function returns a dataframe or json object of StudentEndorsements
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentEndorsements. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentEndorsements.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentEndorsement') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of StudentEndorsements
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentEndorsements <- function(searchConditionsList = NULL, StudentEndorsementID = F, DistrictID = F, StudentID = F, IsReceived = F, EndorsementID = F, IsDeclared = F, IsSignedByStudent = F, IsSignedByGuardian = F, StudentSignedTime = F, GuardianSignedTime = F, NameIDGuardianSignedBy = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsAdminAdded = F, CompletionMethod = F, HasEndorsementOptions = F, IsComplete = F, HasDeclaredEndorsementOptions = F, HasEndorsementOptionsToAddOrDeclare = F, AttachmentComments = F, AttachmentCount = F, DateReceived = F, DateReceivedForDisplay = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "StudentEndorsement", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentEndorsement
	#'
	#' This function returns a dataframe or json object of a StudentEndorsement
	#' @param StudentEndorsementID The ID of the StudentEndorsement to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentEndorsement. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentEndorsement.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentEndorsement') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of StudentEndorsement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentEndorsement <- function(StudentEndorsementID, DistrictID = F, StudentID = F, IsReceived = F, EndorsementID = F, IsDeclared = F, IsSignedByStudent = F, IsSignedByGuardian = F, StudentSignedTime = F, GuardianSignedTime = F, NameIDGuardianSignedBy = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsAdminAdded = F, CompletionMethod = F, HasEndorsementOptions = F, IsComplete = F, HasDeclaredEndorsementOptions = F, HasEndorsementOptionsToAddOrDeclare = F, AttachmentComments = F, AttachmentCount = F, DateReceived = F, DateReceivedForDisplay = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentEndorsementID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "StudentEndorsement", objectId = StudentEndorsementID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentEndorsement
	#'
	#' This function deletes a StudentEndorsement
	#' @param StudentEndorsementID The ID of the StudentEndorsement to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The StudentEndorsementID of the deleted StudentEndorsement.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentEndorsement <- function(StudentEndorsementID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "StudentEndorsement", objectId = StudentEndorsementID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentEndorsement
	#'
	#' This function creates a StudentEndorsement
	#' @param fieldNames The field values to give the created StudentEndorsement. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created StudentEndorsement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentEndorsement <- function(DistrictID = NULL, StudentID = NULL, IsReceived = NULL, EndorsementID = NULL, IsDeclared = NULL, IsSignedByStudent = NULL, IsSignedByGuardian = NULL, StudentSignedTime = NULL, GuardianSignedTime = NULL, NameIDGuardianSignedBy = NULL, IsAdminAdded = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "StudentEndorsement", body = list(DataObject = body), searchFields = append("StudentEndorsementID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentEndorsement
	#'
	#' This function modifies a StudentEndorsement
	#' @param fieldNames The field values to give the modified StudentEndorsement. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified StudentEndorsement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentEndorsement <- function(StudentEndorsementID, DistrictID = NULL, StudentID = NULL, IsReceived = NULL, EndorsementID = NULL, IsDeclared = NULL, IsSignedByStudent = NULL, IsSignedByGuardian = NULL, StudentSignedTime = NULL, GuardianSignedTime = NULL, NameIDGuardianSignedBy = NULL, IsAdminAdded = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "StudentEndorsement", objectId = StudentEndorsementID, body = list(DataObject = body), searchFields = append("StudentEndorsementID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CareerPlanGradeLevels
	#'
	#' This function returns a dataframe or json object of CareerPlanGradeLevels
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CareerPlanGradeLevels. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CareerPlanGradeLevels.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CareerPlanGradeLevel') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of CareerPlanGradeLevels
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCareerPlanGradeLevels <- function(searchConditionsList = NULL, CareerPlanGradeLevelID = F, ConfigDistrictID = F, GradeLevelID = F, IsPriorLevel = F, DisplayName = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "CareerPlanGradeLevel", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CareerPlanGradeLevel
	#'
	#' This function returns a dataframe or json object of a CareerPlanGradeLevel
	#' @param CareerPlanGradeLevelID The ID of the CareerPlanGradeLevel to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CareerPlanGradeLevel. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CareerPlanGradeLevel.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CareerPlanGradeLevel') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of CareerPlanGradeLevel
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCareerPlanGradeLevel <- function(CareerPlanGradeLevelID, ConfigDistrictID = F, GradeLevelID = F, IsPriorLevel = F, DisplayName = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CareerPlanGradeLevelID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "CareerPlanGradeLevel", objectId = CareerPlanGradeLevelID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CareerPlanGradeLevel
	#'
	#' This function deletes a CareerPlanGradeLevel
	#' @param CareerPlanGradeLevelID The ID of the CareerPlanGradeLevel to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The CareerPlanGradeLevelID of the deleted CareerPlanGradeLevel.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCareerPlanGradeLevel <- function(CareerPlanGradeLevelID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "CareerPlanGradeLevel", objectId = CareerPlanGradeLevelID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CareerPlanGradeLevel
	#'
	#' This function creates a CareerPlanGradeLevel
	#' @param fieldNames The field values to give the created CareerPlanGradeLevel. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created CareerPlanGradeLevel
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCareerPlanGradeLevel <- function(ConfigDistrictID = NULL, GradeLevelID = NULL, IsPriorLevel = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "CareerPlanGradeLevel", body = list(DataObject = body), searchFields = append("CareerPlanGradeLevelID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CareerPlanGradeLevel
	#'
	#' This function modifies a CareerPlanGradeLevel
	#' @param fieldNames The field values to give the modified CareerPlanGradeLevel. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified CareerPlanGradeLevel
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCareerPlanGradeLevel <- function(CareerPlanGradeLevelID, ConfigDistrictID = NULL, GradeLevelID = NULL, IsPriorLevel = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "CareerPlanGradeLevel", objectId = CareerPlanGradeLevelID, body = list(DataObject = body), searchFields = append("CareerPlanGradeLevelID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CareerPlanDeclarationTimePeriods
	#'
	#' This function returns a dataframe or json object of CareerPlanDeclarationTimePeriods
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CareerPlanDeclarationTimePeriods. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CareerPlanDeclarationTimePeriods.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CareerPlanDeclarationTimePeriod') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of CareerPlanDeclarationTimePeriods
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCareerPlanDeclarationTimePeriods <- function(searchConditionsList = NULL, CareerPlanDeclarationTimePeriodID = F, EntityID = F, SchoolYearID = F, StartTime = F, EndTime = F, FilterOption = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "CareerPlanDeclarationTimePeriod", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CareerPlanDeclarationTimePeriod
	#'
	#' This function returns a dataframe or json object of a CareerPlanDeclarationTimePeriod
	#' @param CareerPlanDeclarationTimePeriodID The ID of the CareerPlanDeclarationTimePeriod to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CareerPlanDeclarationTimePeriod. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CareerPlanDeclarationTimePeriod.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CareerPlanDeclarationTimePeriod') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of CareerPlanDeclarationTimePeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCareerPlanDeclarationTimePeriod <- function(CareerPlanDeclarationTimePeriodID, EntityID = F, SchoolYearID = F, StartTime = F, EndTime = F, FilterOption = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CareerPlanDeclarationTimePeriodID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "CareerPlanDeclarationTimePeriod", objectId = CareerPlanDeclarationTimePeriodID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CareerPlanDeclarationTimePeriod
	#'
	#' This function deletes a CareerPlanDeclarationTimePeriod
	#' @param CareerPlanDeclarationTimePeriodID The ID of the CareerPlanDeclarationTimePeriod to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The CareerPlanDeclarationTimePeriodID of the deleted CareerPlanDeclarationTimePeriod.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCareerPlanDeclarationTimePeriod <- function(CareerPlanDeclarationTimePeriodID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "CareerPlanDeclarationTimePeriod", objectId = CareerPlanDeclarationTimePeriodID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CareerPlanDeclarationTimePeriod
	#'
	#' This function creates a CareerPlanDeclarationTimePeriod
	#' @param fieldNames The field values to give the created CareerPlanDeclarationTimePeriod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created CareerPlanDeclarationTimePeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCareerPlanDeclarationTimePeriod <- function(EntityID = NULL, SchoolYearID = NULL, StartTime = NULL, EndTime = NULL, FilterOption = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "CareerPlanDeclarationTimePeriod", body = list(DataObject = body), searchFields = append("CareerPlanDeclarationTimePeriodID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CareerPlanDeclarationTimePeriod
	#'
	#' This function modifies a CareerPlanDeclarationTimePeriod
	#' @param fieldNames The field values to give the modified CareerPlanDeclarationTimePeriod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified CareerPlanDeclarationTimePeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCareerPlanDeclarationTimePeriod <- function(CareerPlanDeclarationTimePeriodID, EntityID = NULL, SchoolYearID = NULL, StartTime = NULL, EndTime = NULL, FilterOption = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "CareerPlanDeclarationTimePeriod", objectId = CareerPlanDeclarationTimePeriodID, body = list(DataObject = body), searchFields = append("CareerPlanDeclarationTimePeriodID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CareerPlanDeclarationTimePeriodGradeReferences
	#'
	#' This function returns a dataframe or json object of CareerPlanDeclarationTimePeriodGradeReferences
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CareerPlanDeclarationTimePeriodGradeReferences. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CareerPlanDeclarationTimePeriodGradeReferences.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CareerPlanDeclarationTimePeriodGradeReference') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of CareerPlanDeclarationTimePeriodGradeReferences
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCareerPlanDeclarationTimePeriodGradeReferences <- function(searchConditionsList = NULL, CareerPlanDeclarationTimePeriodGradeReferenceID = F, CareerPlanDeclarationTimePeriodID = F, GradeReferenceID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "CareerPlanDeclarationTimePeriodGradeReference", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CareerPlanDeclarationTimePeriodGradeReference
	#'
	#' This function returns a dataframe or json object of a CareerPlanDeclarationTimePeriodGradeReference
	#' @param CareerPlanDeclarationTimePeriodGradeReferenceID The ID of the CareerPlanDeclarationTimePeriodGradeReference to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CareerPlanDeclarationTimePeriodGradeReference. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CareerPlanDeclarationTimePeriodGradeReference.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CareerPlanDeclarationTimePeriodGradeReference') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of CareerPlanDeclarationTimePeriodGradeReference
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCareerPlanDeclarationTimePeriodGradeReference <- function(CareerPlanDeclarationTimePeriodGradeReferenceID, CareerPlanDeclarationTimePeriodID = F, GradeReferenceID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CareerPlanDeclarationTimePeriodGradeReferenceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "CareerPlanDeclarationTimePeriodGradeReference", objectId = CareerPlanDeclarationTimePeriodGradeReferenceID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CareerPlanDeclarationTimePeriodGradeReference
	#'
	#' This function deletes a CareerPlanDeclarationTimePeriodGradeReference
	#' @param CareerPlanDeclarationTimePeriodGradeReferenceID The ID of the CareerPlanDeclarationTimePeriodGradeReference to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The CareerPlanDeclarationTimePeriodGradeReferenceID of the deleted CareerPlanDeclarationTimePeriodGradeReference.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCareerPlanDeclarationTimePeriodGradeReference <- function(CareerPlanDeclarationTimePeriodGradeReferenceID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "CareerPlanDeclarationTimePeriodGradeReference", objectId = CareerPlanDeclarationTimePeriodGradeReferenceID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CareerPlanDeclarationTimePeriodGradeReference
	#'
	#' This function creates a CareerPlanDeclarationTimePeriodGradeReference
	#' @param fieldNames The field values to give the created CareerPlanDeclarationTimePeriodGradeReference. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created CareerPlanDeclarationTimePeriodGradeReference
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCareerPlanDeclarationTimePeriodGradeReference <- function(CareerPlanDeclarationTimePeriodID = NULL, GradeReferenceID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "CareerPlanDeclarationTimePeriodGradeReference", body = list(DataObject = body), searchFields = append("CareerPlanDeclarationTimePeriodGradeReferenceID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CareerPlanDeclarationTimePeriodGradeReference
	#'
	#' This function modifies a CareerPlanDeclarationTimePeriodGradeReference
	#' @param fieldNames The field values to give the modified CareerPlanDeclarationTimePeriodGradeReference. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified CareerPlanDeclarationTimePeriodGradeReference
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCareerPlanDeclarationTimePeriodGradeReference <- function(CareerPlanDeclarationTimePeriodGradeReferenceID, CareerPlanDeclarationTimePeriodID = NULL, GradeReferenceID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "CareerPlanDeclarationTimePeriodGradeReference", objectId = CareerPlanDeclarationTimePeriodGradeReferenceID, body = list(DataObject = body), searchFields = append("CareerPlanDeclarationTimePeriodGradeReferenceID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CareerPlanDeclarationTimePeriodStudentEntityYears
	#'
	#' This function returns a dataframe or json object of CareerPlanDeclarationTimePeriodStudentEntityYears
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CareerPlanDeclarationTimePeriodStudentEntityYears. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CareerPlanDeclarationTimePeriodStudentEntityYears.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CareerPlanDeclarationTimePeriodStudentEntityYear') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of CareerPlanDeclarationTimePeriodStudentEntityYears
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCareerPlanDeclarationTimePeriodStudentEntityYears <- function(searchConditionsList = NULL, CareerPlanDeclarationTimePeriodStudentEntityYearID = F, CareerPlanDeclarationTimePeriodID = F, StudentEntityYearID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "CareerPlanDeclarationTimePeriodStudentEntityYear", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CareerPlanDeclarationTimePeriodStudentEntityYear
	#'
	#' This function returns a dataframe or json object of a CareerPlanDeclarationTimePeriodStudentEntityYear
	#' @param CareerPlanDeclarationTimePeriodStudentEntityYearID The ID of the CareerPlanDeclarationTimePeriodStudentEntityYear to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CareerPlanDeclarationTimePeriodStudentEntityYear. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CareerPlanDeclarationTimePeriodStudentEntityYear.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CareerPlanDeclarationTimePeriodStudentEntityYear') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of CareerPlanDeclarationTimePeriodStudentEntityYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCareerPlanDeclarationTimePeriodStudentEntityYear <- function(CareerPlanDeclarationTimePeriodStudentEntityYearID, CareerPlanDeclarationTimePeriodID = F, StudentEntityYearID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CareerPlanDeclarationTimePeriodStudentEntityYearID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "CareerPlanDeclarationTimePeriodStudentEntityYear", objectId = CareerPlanDeclarationTimePeriodStudentEntityYearID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CareerPlanDeclarationTimePeriodStudentEntityYear
	#'
	#' This function deletes a CareerPlanDeclarationTimePeriodStudentEntityYear
	#' @param CareerPlanDeclarationTimePeriodStudentEntityYearID The ID of the CareerPlanDeclarationTimePeriodStudentEntityYear to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The CareerPlanDeclarationTimePeriodStudentEntityYearID of the deleted CareerPlanDeclarationTimePeriodStudentEntityYear.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCareerPlanDeclarationTimePeriodStudentEntityYear <- function(CareerPlanDeclarationTimePeriodStudentEntityYearID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "CareerPlanDeclarationTimePeriodStudentEntityYear", objectId = CareerPlanDeclarationTimePeriodStudentEntityYearID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CareerPlanDeclarationTimePeriodStudentEntityYear
	#'
	#' This function creates a CareerPlanDeclarationTimePeriodStudentEntityYear
	#' @param fieldNames The field values to give the created CareerPlanDeclarationTimePeriodStudentEntityYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created CareerPlanDeclarationTimePeriodStudentEntityYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCareerPlanDeclarationTimePeriodStudentEntityYear <- function(CareerPlanDeclarationTimePeriodID = NULL, StudentEntityYearID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "CareerPlanDeclarationTimePeriodStudentEntityYear", body = list(DataObject = body), searchFields = append("CareerPlanDeclarationTimePeriodStudentEntityYearID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CareerPlanDeclarationTimePeriodStudentEntityYear
	#'
	#' This function modifies a CareerPlanDeclarationTimePeriodStudentEntityYear
	#' @param fieldNames The field values to give the modified CareerPlanDeclarationTimePeriodStudentEntityYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified CareerPlanDeclarationTimePeriodStudentEntityYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCareerPlanDeclarationTimePeriodStudentEntityYear <- function(CareerPlanDeclarationTimePeriodStudentEntityYearID, CareerPlanDeclarationTimePeriodID = NULL, StudentEntityYearID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "CareerPlanDeclarationTimePeriodStudentEntityYear", objectId = CareerPlanDeclarationTimePeriodStudentEntityYearID, body = list(DataObject = body), searchFields = append("CareerPlanDeclarationTimePeriodStudentEntityYearID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentCareerPlans
	#'
	#' This function returns a dataframe or json object of StudentCareerPlans
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentCareerPlans. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentCareerPlans.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentCareerPlan') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of StudentCareerPlans
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentCareerPlans <- function(searchConditionsList = NULL, StudentCareerPlanID = F, CurriculumID = F, StudentID = F, StudentSubAreaID = F, StudentCourseRequestID = F, CareerPlanGradeLevelID = F, Credits = F, IsStudentPermittedToChangeGradeLevel = F, IsStudentPermittedToDelete = F, GradeListDisplay = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StudentCareerPlanSummaryID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "StudentCareerPlan", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentCareerPlan
	#'
	#' This function returns a dataframe or json object of a StudentCareerPlan
	#' @param StudentCareerPlanID The ID of the StudentCareerPlan to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentCareerPlan. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentCareerPlan.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentCareerPlan') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of StudentCareerPlan
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentCareerPlan <- function(StudentCareerPlanID, CurriculumID = F, StudentID = F, StudentSubAreaID = F, StudentCourseRequestID = F, CareerPlanGradeLevelID = F, Credits = F, IsStudentPermittedToChangeGradeLevel = F, IsStudentPermittedToDelete = F, GradeListDisplay = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StudentCareerPlanSummaryID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentCareerPlanID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "StudentCareerPlan", objectId = StudentCareerPlanID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentCareerPlan
	#'
	#' This function deletes a StudentCareerPlan
	#' @param StudentCareerPlanID The ID of the StudentCareerPlan to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The StudentCareerPlanID of the deleted StudentCareerPlan.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentCareerPlan <- function(StudentCareerPlanID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "StudentCareerPlan", objectId = StudentCareerPlanID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentCareerPlan
	#'
	#' This function creates a StudentCareerPlan
	#' @param fieldNames The field values to give the created StudentCareerPlan. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created StudentCareerPlan
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentCareerPlan <- function(CurriculumID = NULL, StudentID = NULL, StudentSubAreaID = NULL, StudentCourseRequestID = NULL, CareerPlanGradeLevelID = NULL, Credits = NULL, IsStudentPermittedToChangeGradeLevel = NULL, IsStudentPermittedToDelete = NULL, StudentCareerPlanSummaryID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "StudentCareerPlan", body = list(DataObject = body), searchFields = append("StudentCareerPlanID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentCareerPlan
	#'
	#' This function modifies a StudentCareerPlan
	#' @param fieldNames The field values to give the modified StudentCareerPlan. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified StudentCareerPlan
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentCareerPlan <- function(StudentCareerPlanID, CurriculumID = NULL, StudentID = NULL, StudentSubAreaID = NULL, StudentCourseRequestID = NULL, CareerPlanGradeLevelID = NULL, Credits = NULL, IsStudentPermittedToChangeGradeLevel = NULL, IsStudentPermittedToDelete = NULL, StudentCareerPlanSummaryID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "StudentCareerPlan", objectId = StudentCareerPlanID, body = list(DataObject = body), searchFields = append("StudentCareerPlanID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentEndorsementRequirementCurricula
	#'
	#' This function returns a dataframe or json object of StudentEndorsementRequirementCurricula
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentEndorsementRequirementCurricula. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentEndorsementRequirementCurricula.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentEndorsementRequirementCurriculum') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of StudentEndorsementRequirementCurricula
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentEndorsementRequirementCurricula <- function(searchConditionsList = NULL, StudentEndorsementRequirementCurriculumID = F, StudentEndorsementRequirementID = F, EndorsementRequirementCurriculumID = F, IsComplete = F, OverallCreditsApplied = F, AdvancedCreditsApplied = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "StudentEndorsementRequirementCurriculum", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentEndorsementRequirementCurriculum
	#'
	#' This function returns a dataframe or json object of a StudentEndorsementRequirementCurriculum
	#' @param StudentEndorsementRequirementCurriculumID The ID of the StudentEndorsementRequirementCurriculum to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentEndorsementRequirementCurriculum. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentEndorsementRequirementCurriculum.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentEndorsementRequirementCurriculum') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of StudentEndorsementRequirementCurriculum
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentEndorsementRequirementCurriculum <- function(StudentEndorsementRequirementCurriculumID, StudentEndorsementRequirementID = F, EndorsementRequirementCurriculumID = F, IsComplete = F, OverallCreditsApplied = F, AdvancedCreditsApplied = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentEndorsementRequirementCurriculumID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "StudentEndorsementRequirementCurriculum", objectId = StudentEndorsementRequirementCurriculumID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentEndorsementRequirementCurriculum
	#'
	#' This function deletes a StudentEndorsementRequirementCurriculum
	#' @param StudentEndorsementRequirementCurriculumID The ID of the StudentEndorsementRequirementCurriculum to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The StudentEndorsementRequirementCurriculumID of the deleted StudentEndorsementRequirementCurriculum.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentEndorsementRequirementCurriculum <- function(StudentEndorsementRequirementCurriculumID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "StudentEndorsementRequirementCurriculum", objectId = StudentEndorsementRequirementCurriculumID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentEndorsementRequirementCurriculum
	#'
	#' This function creates a StudentEndorsementRequirementCurriculum
	#' @param fieldNames The field values to give the created StudentEndorsementRequirementCurriculum. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created StudentEndorsementRequirementCurriculum
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentEndorsementRequirementCurriculum <- function(StudentEndorsementRequirementID = NULL, EndorsementRequirementCurriculumID = NULL, IsComplete = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "StudentEndorsementRequirementCurriculum", body = list(DataObject = body), searchFields = append("StudentEndorsementRequirementCurriculumID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentEndorsementRequirementCurriculum
	#'
	#' This function modifies a StudentEndorsementRequirementCurriculum
	#' @param fieldNames The field values to give the modified StudentEndorsementRequirementCurriculum. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified StudentEndorsementRequirementCurriculum
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentEndorsementRequirementCurriculum <- function(StudentEndorsementRequirementCurriculumID, StudentEndorsementRequirementID = NULL, EndorsementRequirementCurriculumID = NULL, IsComplete = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "StudentEndorsementRequirementCurriculum", objectId = StudentEndorsementRequirementCurriculumID, body = list(DataObject = body), searchFields = append("StudentEndorsementRequirementCurriculumID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentEndorsementOptions
	#'
	#' This function returns a dataframe or json object of StudentEndorsementOptions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentEndorsementOptions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentEndorsementOptions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentEndorsementOption') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of StudentEndorsementOptions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentEndorsementOptions <- function(searchConditionsList = NULL, StudentEndorsementOptionID = F, StudentEndorsementID = F, EndorsementOptionID = F, IsDeclared = F, GradPlanInProgress = F, IsComplete = F, OverallCreditsRequired = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReceived = F, AdminAdded = F, DateReceived = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "StudentEndorsementOption", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentEndorsementOption
	#'
	#' This function returns a dataframe or json object of a StudentEndorsementOption
	#' @param StudentEndorsementOptionID The ID of the StudentEndorsementOption to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentEndorsementOption. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentEndorsementOption.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentEndorsementOption') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of StudentEndorsementOption
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentEndorsementOption <- function(StudentEndorsementOptionID, StudentEndorsementID = F, EndorsementOptionID = F, IsDeclared = F, GradPlanInProgress = F, IsComplete = F, OverallCreditsRequired = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReceived = F, AdminAdded = F, DateReceived = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentEndorsementOptionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "StudentEndorsementOption", objectId = StudentEndorsementOptionID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentEndorsementOption
	#'
	#' This function deletes a StudentEndorsementOption
	#' @param StudentEndorsementOptionID The ID of the StudentEndorsementOption to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The StudentEndorsementOptionID of the deleted StudentEndorsementOption.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentEndorsementOption <- function(StudentEndorsementOptionID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "StudentEndorsementOption", objectId = StudentEndorsementOptionID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentEndorsementOption
	#'
	#' This function creates a StudentEndorsementOption
	#' @param fieldNames The field values to give the created StudentEndorsementOption. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created StudentEndorsementOption
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentEndorsementOption <- function(StudentEndorsementID = NULL, EndorsementOptionID = NULL, IsDeclared = NULL, IsComplete = NULL, IsReceived = NULL, AdminAdded = NULL, DateReceived = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "StudentEndorsementOption", body = list(DataObject = body), searchFields = append("StudentEndorsementOptionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentEndorsementOption
	#'
	#' This function modifies a StudentEndorsementOption
	#' @param fieldNames The field values to give the modified StudentEndorsementOption. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified StudentEndorsementOption
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentEndorsementOption <- function(StudentEndorsementOptionID, StudentEndorsementID = NULL, EndorsementOptionID = NULL, IsDeclared = NULL, IsComplete = NULL, IsReceived = NULL, AdminAdded = NULL, DateReceived = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "StudentEndorsementOption", objectId = StudentEndorsementOptionID, body = list(DataObject = body), searchFields = append("StudentEndorsementOptionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentEndorsementRequirements
	#'
	#' This function returns a dataframe or json object of StudentEndorsementRequirements
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentEndorsementRequirements. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentEndorsementRequirements.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentEndorsementRequirement') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of StudentEndorsementRequirements
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentEndorsementRequirements <- function(searchConditionsList = NULL, StudentEndorsementRequirementID = F, StudentEndorsementOptionID = F, EndorsementRequirementID = F, IsComplete = F, OverallCreditsApplied = F, AdvancedCreditsApplied = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "StudentEndorsementRequirement", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentEndorsementRequirement
	#'
	#' This function returns a dataframe or json object of a StudentEndorsementRequirement
	#' @param StudentEndorsementRequirementID The ID of the StudentEndorsementRequirement to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentEndorsementRequirement. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentEndorsementRequirement.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentEndorsementRequirement') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of StudentEndorsementRequirement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentEndorsementRequirement <- function(StudentEndorsementRequirementID, StudentEndorsementOptionID = F, EndorsementRequirementID = F, IsComplete = F, OverallCreditsApplied = F, AdvancedCreditsApplied = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentEndorsementRequirementID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "StudentEndorsementRequirement", objectId = StudentEndorsementRequirementID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentEndorsementRequirement
	#'
	#' This function deletes a StudentEndorsementRequirement
	#' @param StudentEndorsementRequirementID The ID of the StudentEndorsementRequirement to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The StudentEndorsementRequirementID of the deleted StudentEndorsementRequirement.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentEndorsementRequirement <- function(StudentEndorsementRequirementID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "StudentEndorsementRequirement", objectId = StudentEndorsementRequirementID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentEndorsementRequirement
	#'
	#' This function creates a StudentEndorsementRequirement
	#' @param fieldNames The field values to give the created StudentEndorsementRequirement. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created StudentEndorsementRequirement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentEndorsementRequirement <- function(StudentEndorsementOptionID = NULL, EndorsementRequirementID = NULL, IsComplete = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "StudentEndorsementRequirement", body = list(DataObject = body), searchFields = append("StudentEndorsementRequirementID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentEndorsementRequirement
	#'
	#' This function modifies a StudentEndorsementRequirement
	#' @param fieldNames The field values to give the modified StudentEndorsementRequirement. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified StudentEndorsementRequirement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentEndorsementRequirement <- function(StudentEndorsementRequirementID, StudentEndorsementOptionID = NULL, EndorsementRequirementID = NULL, IsComplete = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "StudentEndorsementRequirement", objectId = StudentEndorsementRequirementID, body = list(DataObject = body), searchFields = append("StudentEndorsementRequirementID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentEndorsementRequirementCourseRequests
	#'
	#' This function returns a dataframe or json object of StudentEndorsementRequirementCourseRequests
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentEndorsementRequirementCourseRequests. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentEndorsementRequirementCourseRequests.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentEndorsementRequirementCourseRequest') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of StudentEndorsementRequirementCourseRequests
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentEndorsementRequirementCourseRequests <- function(searchConditionsList = NULL, StudentEndorsementRequirementCourseRequestID = F, StudentEndorsementRequirementID = F, StudentEndorsementRequirementCurriculumID = F, StudentCourseRequestID = F, EndorsementRequirementCurriculumID = F, AppliedOverallCredits = F, AppliedAdvancedCredits = F, ApplyToType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "StudentEndorsementRequirementCourseRequest", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentEndorsementRequirementCourseRequest
	#'
	#' This function returns a dataframe or json object of a StudentEndorsementRequirementCourseRequest
	#' @param StudentEndorsementRequirementCourseRequestID The ID of the StudentEndorsementRequirementCourseRequest to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentEndorsementRequirementCourseRequest. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentEndorsementRequirementCourseRequest.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentEndorsementRequirementCourseRequest') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of StudentEndorsementRequirementCourseRequest
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentEndorsementRequirementCourseRequest <- function(StudentEndorsementRequirementCourseRequestID, StudentEndorsementRequirementID = F, StudentEndorsementRequirementCurriculumID = F, StudentCourseRequestID = F, EndorsementRequirementCurriculumID = F, AppliedOverallCredits = F, AppliedAdvancedCredits = F, ApplyToType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentEndorsementRequirementCourseRequestID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "StudentEndorsementRequirementCourseRequest", objectId = StudentEndorsementRequirementCourseRequestID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentEndorsementRequirementCourseRequest
	#'
	#' This function deletes a StudentEndorsementRequirementCourseRequest
	#' @param StudentEndorsementRequirementCourseRequestID The ID of the StudentEndorsementRequirementCourseRequest to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The StudentEndorsementRequirementCourseRequestID of the deleted StudentEndorsementRequirementCourseRequest.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentEndorsementRequirementCourseRequest <- function(StudentEndorsementRequirementCourseRequestID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "StudentEndorsementRequirementCourseRequest", objectId = StudentEndorsementRequirementCourseRequestID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentEndorsementRequirementCourseRequest
	#'
	#' This function creates a StudentEndorsementRequirementCourseRequest
	#' @param fieldNames The field values to give the created StudentEndorsementRequirementCourseRequest. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created StudentEndorsementRequirementCourseRequest
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentEndorsementRequirementCourseRequest <- function(StudentEndorsementRequirementID = NULL, StudentEndorsementRequirementCurriculumID = NULL, StudentCourseRequestID = NULL, EndorsementRequirementCurriculumID = NULL, AppliedOverallCredits = NULL, AppliedAdvancedCredits = NULL, ApplyToType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "StudentEndorsementRequirementCourseRequest", body = list(DataObject = body), searchFields = append("StudentEndorsementRequirementCourseRequestID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentEndorsementRequirementCourseRequest
	#'
	#' This function modifies a StudentEndorsementRequirementCourseRequest
	#' @param fieldNames The field values to give the modified StudentEndorsementRequirementCourseRequest. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified StudentEndorsementRequirementCourseRequest
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentEndorsementRequirementCourseRequest <- function(StudentEndorsementRequirementCourseRequestID, StudentEndorsementRequirementID = NULL, StudentEndorsementRequirementCurriculumID = NULL, StudentCourseRequestID = NULL, EndorsementRequirementCurriculumID = NULL, AppliedOverallCredits = NULL, AppliedAdvancedCredits = NULL, ApplyToType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "StudentEndorsementRequirementCourseRequest", objectId = StudentEndorsementRequirementCourseRequestID, body = list(DataObject = body), searchFields = append("StudentEndorsementRequirementCourseRequestID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempEndorsementDefaults
	#'
	#' This function returns a dataframe or json object of TempEndorsementDefaults
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempEndorsementDefaults. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempEndorsementDefaults.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempEndorsementDefault') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of TempEndorsementDefaults
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempEndorsementDefaults <- function(searchConditionsList = NULL, TempEndorsementDefaultID = F, EndorsementDefaultID = F, EndorsementID = F, CodeDescription = F, ActionType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PrintOnTranscript = F, Waivable = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "TempEndorsementDefault", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempEndorsementDefault
	#'
	#' This function returns a dataframe or json object of a TempEndorsementDefault
	#' @param TempEndorsementDefaultID The ID of the TempEndorsementDefault to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempEndorsementDefault. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempEndorsementDefault.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempEndorsementDefault') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of TempEndorsementDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempEndorsementDefault <- function(TempEndorsementDefaultID, EndorsementDefaultID = F, EndorsementID = F, CodeDescription = F, ActionType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PrintOnTranscript = F, Waivable = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempEndorsementDefaultID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "TempEndorsementDefault", objectId = TempEndorsementDefaultID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempEndorsementDefault
	#'
	#' This function deletes a TempEndorsementDefault
	#' @param TempEndorsementDefaultID The ID of the TempEndorsementDefault to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The TempEndorsementDefaultID of the deleted TempEndorsementDefault.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempEndorsementDefault <- function(TempEndorsementDefaultID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "TempEndorsementDefault", objectId = TempEndorsementDefaultID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempEndorsementDefault
	#'
	#' This function creates a TempEndorsementDefault
	#' @param fieldNames The field values to give the created TempEndorsementDefault. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created TempEndorsementDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempEndorsementDefault <- function(EndorsementDefaultID = NULL, EndorsementID = NULL, CodeDescription = NULL, ActionType = NULL, PrintOnTranscript = NULL, Waivable = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "TempEndorsementDefault", body = list(DataObject = body), searchFields = append("TempEndorsementDefaultID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempEndorsementDefault
	#'
	#' This function modifies a TempEndorsementDefault
	#' @param fieldNames The field values to give the modified TempEndorsementDefault. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified TempEndorsementDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempEndorsementDefault <- function(TempEndorsementDefaultID, EndorsementDefaultID = NULL, EndorsementID = NULL, CodeDescription = NULL, ActionType = NULL, PrintOnTranscript = NULL, Waivable = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "TempEndorsementDefault", objectId = TempEndorsementDefaultID, body = list(DataObject = body), searchFields = append("TempEndorsementDefaultID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempEndorsementImportErrors
	#'
	#' This function returns a dataframe or json object of TempEndorsementImportErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempEndorsementImportErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempEndorsementImportErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempEndorsementImportError') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of TempEndorsementImportErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempEndorsementImportErrors <- function(searchConditionsList = NULL, TempEndorsementImportErrorID = F, CodeDescription = F, ErrorString = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "TempEndorsementImportError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempEndorsementImportError
	#'
	#' This function returns a dataframe or json object of a TempEndorsementImportError
	#' @param TempEndorsementImportErrorID The ID of the TempEndorsementImportError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempEndorsementImportError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempEndorsementImportError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempEndorsementImportError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of TempEndorsementImportError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempEndorsementImportError <- function(TempEndorsementImportErrorID, CodeDescription = F, ErrorString = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempEndorsementImportErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "TempEndorsementImportError", objectId = TempEndorsementImportErrorID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempEndorsementImportError
	#'
	#' This function deletes a TempEndorsementImportError
	#' @param TempEndorsementImportErrorID The ID of the TempEndorsementImportError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The TempEndorsementImportErrorID of the deleted TempEndorsementImportError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempEndorsementImportError <- function(TempEndorsementImportErrorID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "TempEndorsementImportError", objectId = TempEndorsementImportErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempEndorsementImportError
	#'
	#' This function creates a TempEndorsementImportError
	#' @param fieldNames The field values to give the created TempEndorsementImportError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created TempEndorsementImportError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempEndorsementImportError <- function(CodeDescription = NULL, ErrorString = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "TempEndorsementImportError", body = list(DataObject = body), searchFields = append("TempEndorsementImportErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempEndorsementImportError
	#'
	#' This function modifies a TempEndorsementImportError
	#' @param fieldNames The field values to give the modified TempEndorsementImportError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified TempEndorsementImportError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempEndorsementImportError <- function(TempEndorsementImportErrorID, CodeDescription = NULL, ErrorString = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "TempEndorsementImportError", objectId = TempEndorsementImportErrorID, body = list(DataObject = body), searchFields = append("TempEndorsementImportErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CurriculumClusters
	#'
	#' This function returns a dataframe or json object of CurriculumClusters
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CurriculumClusters. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CurriculumClusters.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CurriculumCluster') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of CurriculumClusters
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCurriculumClusters <- function(searchConditionsList = NULL, CurriculumClusterID = F, DistrictID = F, CurriculumClusterDefaultID = F, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "CurriculumCluster", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CurriculumCluster
	#'
	#' This function returns a dataframe or json object of a CurriculumCluster
	#' @param CurriculumClusterID The ID of the CurriculumCluster to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CurriculumCluster. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CurriculumCluster.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CurriculumCluster') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of CurriculumCluster
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCurriculumCluster <- function(CurriculumClusterID, DistrictID = F, CurriculumClusterDefaultID = F, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CurriculumClusterID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "CurriculumCluster", objectId = CurriculumClusterID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CurriculumCluster
	#'
	#' This function deletes a CurriculumCluster
	#' @param CurriculumClusterID The ID of the CurriculumCluster to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The CurriculumClusterID of the deleted CurriculumCluster.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCurriculumCluster <- function(CurriculumClusterID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "CurriculumCluster", objectId = CurriculumClusterID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CurriculumCluster
	#'
	#' This function creates a CurriculumCluster
	#' @param fieldNames The field values to give the created CurriculumCluster. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created CurriculumCluster
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCurriculumCluster <- function(DistrictID = NULL, CurriculumClusterDefaultID = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "CurriculumCluster", body = list(DataObject = body), searchFields = append("CurriculumClusterID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CurriculumCluster
	#'
	#' This function modifies a CurriculumCluster
	#' @param fieldNames The field values to give the modified CurriculumCluster. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified CurriculumCluster
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCurriculumCluster <- function(CurriculumClusterID, DistrictID = NULL, CurriculumClusterDefaultID = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "CurriculumCluster", objectId = CurriculumClusterID, body = list(DataObject = body), searchFields = append("CurriculumClusterID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CurriculumClusterCurricula
	#'
	#' This function returns a dataframe or json object of CurriculumClusterCurricula
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CurriculumClusterCurricula. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CurriculumClusterCurricula.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CurriculumClusterCurriculum') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of CurriculumClusterCurricula
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCurriculumClusterCurricula <- function(searchConditionsList = NULL, CurriculumClusterCurriculumID = F, CurriculumClusterID = F, CurriculumID = F, GradYearLow = F, GradYearHigh = F, IsAdvancedCredit = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "CurriculumClusterCurriculum", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CurriculumClusterCurriculum
	#'
	#' This function returns a dataframe or json object of a CurriculumClusterCurriculum
	#' @param CurriculumClusterCurriculumID The ID of the CurriculumClusterCurriculum to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CurriculumClusterCurriculum. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CurriculumClusterCurriculum.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CurriculumClusterCurriculum') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of CurriculumClusterCurriculum
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCurriculumClusterCurriculum <- function(CurriculumClusterCurriculumID, CurriculumClusterID = F, CurriculumID = F, GradYearLow = F, GradYearHigh = F, IsAdvancedCredit = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CurriculumClusterCurriculumID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "CurriculumClusterCurriculum", objectId = CurriculumClusterCurriculumID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CurriculumClusterCurriculum
	#'
	#' This function deletes a CurriculumClusterCurriculum
	#' @param CurriculumClusterCurriculumID The ID of the CurriculumClusterCurriculum to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The CurriculumClusterCurriculumID of the deleted CurriculumClusterCurriculum.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCurriculumClusterCurriculum <- function(CurriculumClusterCurriculumID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "CurriculumClusterCurriculum", objectId = CurriculumClusterCurriculumID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CurriculumClusterCurriculum
	#'
	#' This function creates a CurriculumClusterCurriculum
	#' @param fieldNames The field values to give the created CurriculumClusterCurriculum. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created CurriculumClusterCurriculum
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCurriculumClusterCurriculum <- function(CurriculumClusterID = NULL, CurriculumID = NULL, GradYearLow = NULL, GradYearHigh = NULL, IsAdvancedCredit = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "CurriculumClusterCurriculum", body = list(DataObject = body), searchFields = append("CurriculumClusterCurriculumID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CurriculumClusterCurriculum
	#'
	#' This function modifies a CurriculumClusterCurriculum
	#' @param fieldNames The field values to give the modified CurriculumClusterCurriculum. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified CurriculumClusterCurriculum
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCurriculumClusterCurriculum <- function(CurriculumClusterCurriculumID, CurriculumClusterID = NULL, CurriculumID = NULL, GradYearLow = NULL, GradYearHigh = NULL, IsAdvancedCredit = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "CurriculumClusterCurriculum", objectId = CurriculumClusterCurriculumID, body = list(DataObject = body), searchFields = append("CurriculumClusterCurriculumID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CurriculumClusterDefaults
	#'
	#' This function returns a dataframe or json object of CurriculumClusterDefaults
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CurriculumClusterDefaults. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CurriculumClusterDefaults.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CurriculumClusterDefault') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of CurriculumClusterDefaults
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCurriculumClusterDefaults <- function(searchConditionsList = NULL, CurriculumClusterDefaultID = F, Description = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "CurriculumClusterDefault", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CurriculumClusterDefault
	#'
	#' This function returns a dataframe or json object of a CurriculumClusterDefault
	#' @param CurriculumClusterDefaultID The ID of the CurriculumClusterDefault to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CurriculumClusterDefault. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CurriculumClusterDefault.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CurriculumClusterDefault') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of CurriculumClusterDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCurriculumClusterDefault <- function(CurriculumClusterDefaultID, Description = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CurriculumClusterDefaultID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "CurriculumClusterDefault", objectId = CurriculumClusterDefaultID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CurriculumClusterDefault
	#'
	#' This function deletes a CurriculumClusterDefault
	#' @param CurriculumClusterDefaultID The ID of the CurriculumClusterDefault to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The CurriculumClusterDefaultID of the deleted CurriculumClusterDefault.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCurriculumClusterDefault <- function(CurriculumClusterDefaultID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "CurriculumClusterDefault", objectId = CurriculumClusterDefaultID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CurriculumClusterDefault
	#'
	#' This function creates a CurriculumClusterDefault
	#' @param fieldNames The field values to give the created CurriculumClusterDefault. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created CurriculumClusterDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCurriculumClusterDefault <- function(Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "CurriculumClusterDefault", body = list(DataObject = body), searchFields = append("CurriculumClusterDefaultID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CurriculumClusterDefault
	#'
	#' This function modifies a CurriculumClusterDefault
	#' @param fieldNames The field values to give the modified CurriculumClusterDefault. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified CurriculumClusterDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCurriculumClusterDefault <- function(CurriculumClusterDefaultID, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "CurriculumClusterDefault", objectId = CurriculumClusterDefaultID, body = list(DataObject = body), searchFields = append("CurriculumClusterDefaultID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EndorsementDefaults
	#'
	#' This function returns a dataframe or json object of EndorsementDefaults
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EndorsementDefaults. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EndorsementDefaults.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EndorsementDefault') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of EndorsementDefaults
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEndorsementDefaults <- function(searchConditionsList = NULL, EndorsementDefaultID = F, Code = F, Description = F, IsActive = F, IsDeclarable = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PrintOnTranscript = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "EndorsementDefault", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EndorsementDefault
	#'
	#' This function returns a dataframe or json object of an EndorsementDefault
	#' @param EndorsementDefaultID The ID of the EndorsementDefault to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EndorsementDefault. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EndorsementDefault.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EndorsementDefault') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of EndorsementDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEndorsementDefault <- function(EndorsementDefaultID, Code = F, Description = F, IsActive = F, IsDeclarable = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PrintOnTranscript = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EndorsementDefaultID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "EndorsementDefault", objectId = EndorsementDefaultID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EndorsementDefault
	#'
	#' This function deletes an EndorsementDefault
	#' @param EndorsementDefaultID The ID of the EndorsementDefault to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The EndorsementDefaultID of the deleted EndorsementDefault.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEndorsementDefault <- function(EndorsementDefaultID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "EndorsementDefault", objectId = EndorsementDefaultID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EndorsementDefault
	#'
	#' This function creates an EndorsementDefault
	#' @param fieldNames The field values to give the created EndorsementDefault. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created EndorsementDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEndorsementDefault <- function(Code = NULL, Description = NULL, IsActive = NULL, IsDeclarable = NULL, PrintOnTranscript = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "EndorsementDefault", body = list(DataObject = body), searchFields = append("EndorsementDefaultID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EndorsementDefault
	#'
	#' This function modifies an EndorsementDefault
	#' @param fieldNames The field values to give the modified EndorsementDefault. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified EndorsementDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEndorsementDefault <- function(EndorsementDefaultID, Code = NULL, Description = NULL, IsActive = NULL, IsDeclarable = NULL, PrintOnTranscript = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "EndorsementDefault", objectId = EndorsementDefaultID, body = list(DataObject = body), searchFields = append("EndorsementDefaultID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EndorsementOptions
	#'
	#' This function returns a dataframe or json object of EndorsementOptions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EndorsementOptions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EndorsementOptions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EndorsementOption') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of EndorsementOptions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEndorsementOptions <- function(searchConditionsList = NULL, EndorsementOptionID = F, EndorsementID = F, EndorsementOptionDefaultID = F, Code = F, Description = F, OrderNumber = F, MustCompleteGradPlan = F, GradYearLow = F, GradYearHigh = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "EndorsementOption", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EndorsementOption
	#'
	#' This function returns a dataframe or json object of an EndorsementOption
	#' @param EndorsementOptionID The ID of the EndorsementOption to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EndorsementOption. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EndorsementOption.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EndorsementOption') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of EndorsementOption
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEndorsementOption <- function(EndorsementOptionID, EndorsementID = F, EndorsementOptionDefaultID = F, Code = F, Description = F, OrderNumber = F, MustCompleteGradPlan = F, GradYearLow = F, GradYearHigh = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EndorsementOptionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "EndorsementOption", objectId = EndorsementOptionID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EndorsementOption
	#'
	#' This function deletes an EndorsementOption
	#' @param EndorsementOptionID The ID of the EndorsementOption to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The EndorsementOptionID of the deleted EndorsementOption.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEndorsementOption <- function(EndorsementOptionID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "EndorsementOption", objectId = EndorsementOptionID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EndorsementOption
	#'
	#' This function creates an EndorsementOption
	#' @param fieldNames The field values to give the created EndorsementOption. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created EndorsementOption
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEndorsementOption <- function(EndorsementID = NULL, EndorsementOptionDefaultID = NULL, Code = NULL, Description = NULL, OrderNumber = NULL, MustCompleteGradPlan = NULL, GradYearLow = NULL, GradYearHigh = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "EndorsementOption", body = list(DataObject = body), searchFields = append("EndorsementOptionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EndorsementOption
	#'
	#' This function modifies an EndorsementOption
	#' @param fieldNames The field values to give the modified EndorsementOption. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified EndorsementOption
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEndorsementOption <- function(EndorsementOptionID, EndorsementID = NULL, EndorsementOptionDefaultID = NULL, Code = NULL, Description = NULL, OrderNumber = NULL, MustCompleteGradPlan = NULL, GradYearLow = NULL, GradYearHigh = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "EndorsementOption", objectId = EndorsementOptionID, body = list(DataObject = body), searchFields = append("EndorsementOptionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EndorsementOptionDefaults
	#'
	#' This function returns a dataframe or json object of EndorsementOptionDefaults
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EndorsementOptionDefaults. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EndorsementOptionDefaults.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EndorsementOptionDefault') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of EndorsementOptionDefaults
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEndorsementOptionDefaults <- function(searchConditionsList = NULL, EndorsementOptionDefaultID = F, EndorsementDefaultID = F, Code = F, Description = F, OrderNumber = F, MustCompleteGradPlan = F, GradYearLow = F, GradYearHigh = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "EndorsementOptionDefault", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EndorsementOptionDefault
	#'
	#' This function returns a dataframe or json object of an EndorsementOptionDefault
	#' @param EndorsementOptionDefaultID The ID of the EndorsementOptionDefault to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EndorsementOptionDefault. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EndorsementOptionDefault.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EndorsementOptionDefault') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of EndorsementOptionDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEndorsementOptionDefault <- function(EndorsementOptionDefaultID, EndorsementDefaultID = F, Code = F, Description = F, OrderNumber = F, MustCompleteGradPlan = F, GradYearLow = F, GradYearHigh = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EndorsementOptionDefaultID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "EndorsementOptionDefault", objectId = EndorsementOptionDefaultID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EndorsementOptionDefault
	#'
	#' This function deletes an EndorsementOptionDefault
	#' @param EndorsementOptionDefaultID The ID of the EndorsementOptionDefault to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The EndorsementOptionDefaultID of the deleted EndorsementOptionDefault.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEndorsementOptionDefault <- function(EndorsementOptionDefaultID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "EndorsementOptionDefault", objectId = EndorsementOptionDefaultID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EndorsementOptionDefault
	#'
	#' This function creates an EndorsementOptionDefault
	#' @param fieldNames The field values to give the created EndorsementOptionDefault. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created EndorsementOptionDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEndorsementOptionDefault <- function(EndorsementDefaultID = NULL, Code = NULL, Description = NULL, OrderNumber = NULL, MustCompleteGradPlan = NULL, GradYearLow = NULL, GradYearHigh = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "EndorsementOptionDefault", body = list(DataObject = body), searchFields = append("EndorsementOptionDefaultID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EndorsementOptionDefault
	#'
	#' This function modifies an EndorsementOptionDefault
	#' @param fieldNames The field values to give the modified EndorsementOptionDefault. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified EndorsementOptionDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEndorsementOptionDefault <- function(EndorsementOptionDefaultID, EndorsementDefaultID = NULL, Code = NULL, Description = NULL, OrderNumber = NULL, MustCompleteGradPlan = NULL, GradYearLow = NULL, GradYearHigh = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "EndorsementOptionDefault", objectId = EndorsementOptionDefaultID, body = list(DataObject = body), searchFields = append("EndorsementOptionDefaultID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EndorsementRequirements
	#'
	#' This function returns a dataframe or json object of EndorsementRequirements
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EndorsementRequirements. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EndorsementRequirements.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EndorsementRequirement') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of EndorsementRequirements
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEndorsementRequirements <- function(searchConditionsList = NULL, EndorsementRequirementID = F, EndorsementOptionID = F, EndorsementRequirementDefaultID = F, Description = F, OverallCreditsRequired = F, AdvancedCreditsRequired = F, OrderNumber = F, MustFulfillAllCurriculumClusters = F, MinimumClusterLimit = F, UseMaximumClusterLimit = F, MaximumClusterLimit = F, RequirementType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, RequirementAssessmentType = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "EndorsementRequirement", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EndorsementRequirement
	#'
	#' This function returns a dataframe or json object of an EndorsementRequirement
	#' @param EndorsementRequirementID The ID of the EndorsementRequirement to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EndorsementRequirement. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EndorsementRequirement.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EndorsementRequirement') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of EndorsementRequirement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEndorsementRequirement <- function(EndorsementRequirementID, EndorsementOptionID = F, EndorsementRequirementDefaultID = F, Description = F, OverallCreditsRequired = F, AdvancedCreditsRequired = F, OrderNumber = F, MustFulfillAllCurriculumClusters = F, MinimumClusterLimit = F, UseMaximumClusterLimit = F, MaximumClusterLimit = F, RequirementType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, RequirementAssessmentType = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EndorsementRequirementID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "EndorsementRequirement", objectId = EndorsementRequirementID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EndorsementRequirement
	#'
	#' This function deletes an EndorsementRequirement
	#' @param EndorsementRequirementID The ID of the EndorsementRequirement to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The EndorsementRequirementID of the deleted EndorsementRequirement.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEndorsementRequirement <- function(EndorsementRequirementID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "EndorsementRequirement", objectId = EndorsementRequirementID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EndorsementRequirement
	#'
	#' This function creates an EndorsementRequirement
	#' @param fieldNames The field values to give the created EndorsementRequirement. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created EndorsementRequirement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEndorsementRequirement <- function(EndorsementOptionID = NULL, EndorsementRequirementDefaultID = NULL, Description = NULL, OverallCreditsRequired = NULL, AdvancedCreditsRequired = NULL, OrderNumber = NULL, MustFulfillAllCurriculumClusters = NULL, MinimumClusterLimit = NULL, UseMaximumClusterLimit = NULL, MaximumClusterLimit = NULL, RequirementType = NULL, RequirementAssessmentType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "EndorsementRequirement", body = list(DataObject = body), searchFields = append("EndorsementRequirementID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EndorsementRequirement
	#'
	#' This function modifies an EndorsementRequirement
	#' @param fieldNames The field values to give the modified EndorsementRequirement. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified EndorsementRequirement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEndorsementRequirement <- function(EndorsementRequirementID, EndorsementOptionID = NULL, EndorsementRequirementDefaultID = NULL, Description = NULL, OverallCreditsRequired = NULL, AdvancedCreditsRequired = NULL, OrderNumber = NULL, MustFulfillAllCurriculumClusters = NULL, MinimumClusterLimit = NULL, UseMaximumClusterLimit = NULL, MaximumClusterLimit = NULL, RequirementType = NULL, RequirementAssessmentType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "EndorsementRequirement", objectId = EndorsementRequirementID, body = list(DataObject = body), searchFields = append("EndorsementRequirementID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EndorsementRequirementCurricula
	#'
	#' This function returns a dataframe or json object of EndorsementRequirementCurricula
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EndorsementRequirementCurricula. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EndorsementRequirementCurricula.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EndorsementRequirementCurriculum') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of EndorsementRequirementCurricula
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEndorsementRequirementCurricula <- function(searchConditionsList = NULL, EndorsementRequirementCurriculumID = F, EndorsementRequirementID = F, EndorsementRequirementCurriculumDefaultID = F, CurriculumClusterID = F, CreditsRequired = F, AdvancedCreditsRequired = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "EndorsementRequirementCurriculum", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EndorsementRequirementCurriculum
	#'
	#' This function returns a dataframe or json object of an EndorsementRequirementCurriculum
	#' @param EndorsementRequirementCurriculumID The ID of the EndorsementRequirementCurriculum to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EndorsementRequirementCurriculum. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EndorsementRequirementCurriculum.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EndorsementRequirementCurriculum') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of EndorsementRequirementCurriculum
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEndorsementRequirementCurriculum <- function(EndorsementRequirementCurriculumID, EndorsementRequirementID = F, EndorsementRequirementCurriculumDefaultID = F, CurriculumClusterID = F, CreditsRequired = F, AdvancedCreditsRequired = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EndorsementRequirementCurriculumID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "EndorsementRequirementCurriculum", objectId = EndorsementRequirementCurriculumID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EndorsementRequirementCurriculum
	#'
	#' This function deletes an EndorsementRequirementCurriculum
	#' @param EndorsementRequirementCurriculumID The ID of the EndorsementRequirementCurriculum to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The EndorsementRequirementCurriculumID of the deleted EndorsementRequirementCurriculum.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEndorsementRequirementCurriculum <- function(EndorsementRequirementCurriculumID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "EndorsementRequirementCurriculum", objectId = EndorsementRequirementCurriculumID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EndorsementRequirementCurriculum
	#'
	#' This function creates an EndorsementRequirementCurriculum
	#' @param fieldNames The field values to give the created EndorsementRequirementCurriculum. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created EndorsementRequirementCurriculum
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEndorsementRequirementCurriculum <- function(EndorsementRequirementID = NULL, EndorsementRequirementCurriculumDefaultID = NULL, CurriculumClusterID = NULL, CreditsRequired = NULL, AdvancedCreditsRequired = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "EndorsementRequirementCurriculum", body = list(DataObject = body), searchFields = append("EndorsementRequirementCurriculumID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EndorsementRequirementCurriculum
	#'
	#' This function modifies an EndorsementRequirementCurriculum
	#' @param fieldNames The field values to give the modified EndorsementRequirementCurriculum. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified EndorsementRequirementCurriculum
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEndorsementRequirementCurriculum <- function(EndorsementRequirementCurriculumID, EndorsementRequirementID = NULL, EndorsementRequirementCurriculumDefaultID = NULL, CurriculumClusterID = NULL, CreditsRequired = NULL, AdvancedCreditsRequired = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "EndorsementRequirementCurriculum", objectId = EndorsementRequirementCurriculumID, body = list(DataObject = body), searchFields = append("EndorsementRequirementCurriculumID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EndorsementRequirementCurriculumDefaults
	#'
	#' This function returns a dataframe or json object of EndorsementRequirementCurriculumDefaults
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EndorsementRequirementCurriculumDefaults. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EndorsementRequirementCurriculumDefaults.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EndorsementRequirementCurriculumDefault') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of EndorsementRequirementCurriculumDefaults
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEndorsementRequirementCurriculumDefaults <- function(searchConditionsList = NULL, EndorsementRequirementCurriculumDefaultID = F, EndorsementRequirementDefaultID = F, CurriculumClusterDefaultID = F, CreditsRequired = F, AdvancedCreditsRequired = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "EndorsementRequirementCurriculumDefault", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EndorsementRequirementCurriculumDefault
	#'
	#' This function returns a dataframe or json object of an EndorsementRequirementCurriculumDefault
	#' @param EndorsementRequirementCurriculumDefaultID The ID of the EndorsementRequirementCurriculumDefault to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EndorsementRequirementCurriculumDefault. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EndorsementRequirementCurriculumDefault.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EndorsementRequirementCurriculumDefault') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of EndorsementRequirementCurriculumDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEndorsementRequirementCurriculumDefault <- function(EndorsementRequirementCurriculumDefaultID, EndorsementRequirementDefaultID = F, CurriculumClusterDefaultID = F, CreditsRequired = F, AdvancedCreditsRequired = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EndorsementRequirementCurriculumDefaultID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "EndorsementRequirementCurriculumDefault", objectId = EndorsementRequirementCurriculumDefaultID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EndorsementRequirementCurriculumDefault
	#'
	#' This function deletes an EndorsementRequirementCurriculumDefault
	#' @param EndorsementRequirementCurriculumDefaultID The ID of the EndorsementRequirementCurriculumDefault to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The EndorsementRequirementCurriculumDefaultID of the deleted EndorsementRequirementCurriculumDefault.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEndorsementRequirementCurriculumDefault <- function(EndorsementRequirementCurriculumDefaultID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "EndorsementRequirementCurriculumDefault", objectId = EndorsementRequirementCurriculumDefaultID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EndorsementRequirementCurriculumDefault
	#'
	#' This function creates an EndorsementRequirementCurriculumDefault
	#' @param fieldNames The field values to give the created EndorsementRequirementCurriculumDefault. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created EndorsementRequirementCurriculumDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEndorsementRequirementCurriculumDefault <- function(EndorsementRequirementDefaultID = NULL, CurriculumClusterDefaultID = NULL, CreditsRequired = NULL, AdvancedCreditsRequired = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "EndorsementRequirementCurriculumDefault", body = list(DataObject = body), searchFields = append("EndorsementRequirementCurriculumDefaultID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EndorsementRequirementCurriculumDefault
	#'
	#' This function modifies an EndorsementRequirementCurriculumDefault
	#' @param fieldNames The field values to give the modified EndorsementRequirementCurriculumDefault. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified EndorsementRequirementCurriculumDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEndorsementRequirementCurriculumDefault <- function(EndorsementRequirementCurriculumDefaultID, EndorsementRequirementDefaultID = NULL, CurriculumClusterDefaultID = NULL, CreditsRequired = NULL, AdvancedCreditsRequired = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "EndorsementRequirementCurriculumDefault", objectId = EndorsementRequirementCurriculumDefaultID, body = list(DataObject = body), searchFields = append("EndorsementRequirementCurriculumDefaultID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EndorsementRequirementDefaults
	#'
	#' This function returns a dataframe or json object of EndorsementRequirementDefaults
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EndorsementRequirementDefaults. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EndorsementRequirementDefaults.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EndorsementRequirementDefault') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of EndorsementRequirementDefaults
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEndorsementRequirementDefaults <- function(searchConditionsList = NULL, EndorsementRequirementDefaultID = F, EndorsementOptionDefaultID = F, Description = F, OverallCreditsRequired = F, AdvancedCreditsRequired = F, MustFulfillAllCurriculumClusters = F, MinimumClusterLimit = F, UseMaximumClusterLimit = F, MaximumClusterLimit = F, OrderNumber = F, SkywardID = F, SkywardHash = F, RequirementType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, RequirementAssessmentType = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "EndorsementRequirementDefault", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EndorsementRequirementDefault
	#'
	#' This function returns a dataframe or json object of an EndorsementRequirementDefault
	#' @param EndorsementRequirementDefaultID The ID of the EndorsementRequirementDefault to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EndorsementRequirementDefault. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EndorsementRequirementDefault.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EndorsementRequirementDefault') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of EndorsementRequirementDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEndorsementRequirementDefault <- function(EndorsementRequirementDefaultID, EndorsementOptionDefaultID = F, Description = F, OverallCreditsRequired = F, AdvancedCreditsRequired = F, MustFulfillAllCurriculumClusters = F, MinimumClusterLimit = F, UseMaximumClusterLimit = F, MaximumClusterLimit = F, OrderNumber = F, SkywardID = F, SkywardHash = F, RequirementType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, RequirementAssessmentType = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EndorsementRequirementDefaultID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "EndorsementRequirementDefault", objectId = EndorsementRequirementDefaultID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EndorsementRequirementDefault
	#'
	#' This function deletes an EndorsementRequirementDefault
	#' @param EndorsementRequirementDefaultID The ID of the EndorsementRequirementDefault to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The EndorsementRequirementDefaultID of the deleted EndorsementRequirementDefault.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEndorsementRequirementDefault <- function(EndorsementRequirementDefaultID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "EndorsementRequirementDefault", objectId = EndorsementRequirementDefaultID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EndorsementRequirementDefault
	#'
	#' This function creates an EndorsementRequirementDefault
	#' @param fieldNames The field values to give the created EndorsementRequirementDefault. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created EndorsementRequirementDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEndorsementRequirementDefault <- function(EndorsementOptionDefaultID = NULL, Description = NULL, OverallCreditsRequired = NULL, AdvancedCreditsRequired = NULL, MustFulfillAllCurriculumClusters = NULL, MinimumClusterLimit = NULL, UseMaximumClusterLimit = NULL, MaximumClusterLimit = NULL, OrderNumber = NULL, RequirementType = NULL, RequirementAssessmentType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "EndorsementRequirementDefault", body = list(DataObject = body), searchFields = append("EndorsementRequirementDefaultID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EndorsementRequirementDefault
	#'
	#' This function modifies an EndorsementRequirementDefault
	#' @param fieldNames The field values to give the modified EndorsementRequirementDefault. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified EndorsementRequirementDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEndorsementRequirementDefault <- function(EndorsementRequirementDefaultID, EndorsementOptionDefaultID = NULL, Description = NULL, OverallCreditsRequired = NULL, AdvancedCreditsRequired = NULL, MustFulfillAllCurriculumClusters = NULL, MinimumClusterLimit = NULL, UseMaximumClusterLimit = NULL, MaximumClusterLimit = NULL, OrderNumber = NULL, RequirementType = NULL, RequirementAssessmentType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "EndorsementRequirementDefault", objectId = EndorsementRequirementDefaultID, body = list(DataObject = body), searchFields = append("EndorsementRequirementDefaultID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EndorsementRequirementAssessments
	#'
	#' This function returns a dataframe or json object of EndorsementRequirementAssessments
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EndorsementRequirementAssessments. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EndorsementRequirementAssessments.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EndorsementRequirementAssessment') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of EndorsementRequirementAssessments
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEndorsementRequirementAssessments <- function(searchConditionsList = NULL, EndorsementRequirementAssessmentID = F, EndorsementRequirementID = F, EndorsementRequirementAssessmentDefaultID = F, TestType = F, TestVersion = F, ClusterType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "EndorsementRequirementAssessment", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EndorsementRequirementAssessment
	#'
	#' This function returns a dataframe or json object of an EndorsementRequirementAssessment
	#' @param EndorsementRequirementAssessmentID The ID of the EndorsementRequirementAssessment to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EndorsementRequirementAssessment. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EndorsementRequirementAssessment.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EndorsementRequirementAssessment') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of EndorsementRequirementAssessment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEndorsementRequirementAssessment <- function(EndorsementRequirementAssessmentID, EndorsementRequirementID = F, EndorsementRequirementAssessmentDefaultID = F, TestType = F, TestVersion = F, ClusterType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EndorsementRequirementAssessmentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "EndorsementRequirementAssessment", objectId = EndorsementRequirementAssessmentID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EndorsementRequirementAssessment
	#'
	#' This function deletes an EndorsementRequirementAssessment
	#' @param EndorsementRequirementAssessmentID The ID of the EndorsementRequirementAssessment to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The EndorsementRequirementAssessmentID of the deleted EndorsementRequirementAssessment.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEndorsementRequirementAssessment <- function(EndorsementRequirementAssessmentID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "EndorsementRequirementAssessment", objectId = EndorsementRequirementAssessmentID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EndorsementRequirementAssessment
	#'
	#' This function creates an EndorsementRequirementAssessment
	#' @param fieldNames The field values to give the created EndorsementRequirementAssessment. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created EndorsementRequirementAssessment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEndorsementRequirementAssessment <- function(EndorsementRequirementID = NULL, EndorsementRequirementAssessmentDefaultID = NULL, TestType = NULL, TestVersion = NULL, ClusterType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "EndorsementRequirementAssessment", body = list(DataObject = body), searchFields = append("EndorsementRequirementAssessmentID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EndorsementRequirementAssessment
	#'
	#' This function modifies an EndorsementRequirementAssessment
	#' @param fieldNames The field values to give the modified EndorsementRequirementAssessment. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified EndorsementRequirementAssessment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEndorsementRequirementAssessment <- function(EndorsementRequirementAssessmentID, EndorsementRequirementID = NULL, EndorsementRequirementAssessmentDefaultID = NULL, TestType = NULL, TestVersion = NULL, ClusterType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "EndorsementRequirementAssessment", objectId = EndorsementRequirementAssessmentID, body = list(DataObject = body), searchFields = append("EndorsementRequirementAssessmentID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EndorsementRequirementAssessmentClusters
	#'
	#' This function returns a dataframe or json object of EndorsementRequirementAssessmentClusters
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EndorsementRequirementAssessmentClusters. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EndorsementRequirementAssessmentClusters.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EndorsementRequirementAssessmentCluster') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of EndorsementRequirementAssessmentClusters
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEndorsementRequirementAssessmentClusters <- function(searchConditionsList = NULL, EndorsementRequirementAssessmentClusterID = F, EndorsementRequirementAssessmentID = F, EndorsementRequirementAssessmentClusterDefaultID = F, ClusterScoreType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "EndorsementRequirementAssessmentCluster", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EndorsementRequirementAssessmentCluster
	#'
	#' This function returns a dataframe or json object of an EndorsementRequirementAssessmentCluster
	#' @param EndorsementRequirementAssessmentClusterID The ID of the EndorsementRequirementAssessmentCluster to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EndorsementRequirementAssessmentCluster. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EndorsementRequirementAssessmentCluster.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EndorsementRequirementAssessmentCluster') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of EndorsementRequirementAssessmentCluster
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEndorsementRequirementAssessmentCluster <- function(EndorsementRequirementAssessmentClusterID, EndorsementRequirementAssessmentID = F, EndorsementRequirementAssessmentClusterDefaultID = F, ClusterScoreType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EndorsementRequirementAssessmentClusterID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "EndorsementRequirementAssessmentCluster", objectId = EndorsementRequirementAssessmentClusterID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EndorsementRequirementAssessmentCluster
	#'
	#' This function deletes an EndorsementRequirementAssessmentCluster
	#' @param EndorsementRequirementAssessmentClusterID The ID of the EndorsementRequirementAssessmentCluster to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The EndorsementRequirementAssessmentClusterID of the deleted EndorsementRequirementAssessmentCluster.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEndorsementRequirementAssessmentCluster <- function(EndorsementRequirementAssessmentClusterID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "EndorsementRequirementAssessmentCluster", objectId = EndorsementRequirementAssessmentClusterID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EndorsementRequirementAssessmentCluster
	#'
	#' This function creates an EndorsementRequirementAssessmentCluster
	#' @param fieldNames The field values to give the created EndorsementRequirementAssessmentCluster. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created EndorsementRequirementAssessmentCluster
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEndorsementRequirementAssessmentCluster <- function(EndorsementRequirementAssessmentID = NULL, EndorsementRequirementAssessmentClusterDefaultID = NULL, ClusterScoreType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "EndorsementRequirementAssessmentCluster", body = list(DataObject = body), searchFields = append("EndorsementRequirementAssessmentClusterID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EndorsementRequirementAssessmentCluster
	#'
	#' This function modifies an EndorsementRequirementAssessmentCluster
	#' @param fieldNames The field values to give the modified EndorsementRequirementAssessmentCluster. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified EndorsementRequirementAssessmentCluster
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEndorsementRequirementAssessmentCluster <- function(EndorsementRequirementAssessmentClusterID, EndorsementRequirementAssessmentID = NULL, EndorsementRequirementAssessmentClusterDefaultID = NULL, ClusterScoreType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "EndorsementRequirementAssessmentCluster", objectId = EndorsementRequirementAssessmentClusterID, body = list(DataObject = body), searchFields = append("EndorsementRequirementAssessmentClusterID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EndorsementRequirementAssessmentClusterDefaults
	#'
	#' This function returns a dataframe or json object of EndorsementRequirementAssessmentClusterDefaults
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EndorsementRequirementAssessmentClusterDefaults. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EndorsementRequirementAssessmentClusterDefaults.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EndorsementRequirementAssessmentClusterDefault') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of EndorsementRequirementAssessmentClusterDefaults
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEndorsementRequirementAssessmentClusterDefaults <- function(searchConditionsList = NULL, EndorsementRequirementAssessmentClusterDefaultID = F, EndorsementRequirementAssessmentDefaultID = F, SkywardID = F, SkywardHash = F, ClusterScoreType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "EndorsementRequirementAssessmentClusterDefault", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EndorsementRequirementAssessmentClusterDefault
	#'
	#' This function returns a dataframe or json object of an EndorsementRequirementAssessmentClusterDefault
	#' @param EndorsementRequirementAssessmentClusterDefaultID The ID of the EndorsementRequirementAssessmentClusterDefault to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EndorsementRequirementAssessmentClusterDefault. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EndorsementRequirementAssessmentClusterDefault.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EndorsementRequirementAssessmentClusterDefault') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of EndorsementRequirementAssessmentClusterDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEndorsementRequirementAssessmentClusterDefault <- function(EndorsementRequirementAssessmentClusterDefaultID, EndorsementRequirementAssessmentDefaultID = F, SkywardID = F, SkywardHash = F, ClusterScoreType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EndorsementRequirementAssessmentClusterDefaultID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "EndorsementRequirementAssessmentClusterDefault", objectId = EndorsementRequirementAssessmentClusterDefaultID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EndorsementRequirementAssessmentClusterDefault
	#'
	#' This function deletes an EndorsementRequirementAssessmentClusterDefault
	#' @param EndorsementRequirementAssessmentClusterDefaultID The ID of the EndorsementRequirementAssessmentClusterDefault to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The EndorsementRequirementAssessmentClusterDefaultID of the deleted EndorsementRequirementAssessmentClusterDefault.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEndorsementRequirementAssessmentClusterDefault <- function(EndorsementRequirementAssessmentClusterDefaultID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "EndorsementRequirementAssessmentClusterDefault", objectId = EndorsementRequirementAssessmentClusterDefaultID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EndorsementRequirementAssessmentClusterDefault
	#'
	#' This function creates an EndorsementRequirementAssessmentClusterDefault
	#' @param fieldNames The field values to give the created EndorsementRequirementAssessmentClusterDefault. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created EndorsementRequirementAssessmentClusterDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEndorsementRequirementAssessmentClusterDefault <- function(EndorsementRequirementAssessmentDefaultID = NULL, ClusterScoreType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "EndorsementRequirementAssessmentClusterDefault", body = list(DataObject = body), searchFields = append("EndorsementRequirementAssessmentClusterDefaultID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EndorsementRequirementAssessmentClusterDefault
	#'
	#' This function modifies an EndorsementRequirementAssessmentClusterDefault
	#' @param fieldNames The field values to give the modified EndorsementRequirementAssessmentClusterDefault. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified EndorsementRequirementAssessmentClusterDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEndorsementRequirementAssessmentClusterDefault <- function(EndorsementRequirementAssessmentClusterDefaultID, EndorsementRequirementAssessmentDefaultID = NULL, ClusterScoreType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "EndorsementRequirementAssessmentClusterDefault", objectId = EndorsementRequirementAssessmentClusterDefaultID, body = list(DataObject = body), searchFields = append("EndorsementRequirementAssessmentClusterDefaultID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EndorsementRequirementAssessmentDefaults
	#'
	#' This function returns a dataframe or json object of EndorsementRequirementAssessmentDefaults
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EndorsementRequirementAssessmentDefaults. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EndorsementRequirementAssessmentDefaults.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EndorsementRequirementAssessmentDefault') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of EndorsementRequirementAssessmentDefaults
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEndorsementRequirementAssessmentDefaults <- function(searchConditionsList = NULL, EndorsementRequirementAssessmentDefaultID = F, EndorsementRequirementDefaultID = F, TestType = F, TestVersion = F, SkywardID = F, SkywardHash = F, ClusterType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "EndorsementRequirementAssessmentDefault", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EndorsementRequirementAssessmentDefault
	#'
	#' This function returns a dataframe or json object of an EndorsementRequirementAssessmentDefault
	#' @param EndorsementRequirementAssessmentDefaultID The ID of the EndorsementRequirementAssessmentDefault to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EndorsementRequirementAssessmentDefault. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EndorsementRequirementAssessmentDefault.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EndorsementRequirementAssessmentDefault') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of EndorsementRequirementAssessmentDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEndorsementRequirementAssessmentDefault <- function(EndorsementRequirementAssessmentDefaultID, EndorsementRequirementDefaultID = F, TestType = F, TestVersion = F, SkywardID = F, SkywardHash = F, ClusterType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EndorsementRequirementAssessmentDefaultID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "EndorsementRequirementAssessmentDefault", objectId = EndorsementRequirementAssessmentDefaultID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EndorsementRequirementAssessmentDefault
	#'
	#' This function deletes an EndorsementRequirementAssessmentDefault
	#' @param EndorsementRequirementAssessmentDefaultID The ID of the EndorsementRequirementAssessmentDefault to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The EndorsementRequirementAssessmentDefaultID of the deleted EndorsementRequirementAssessmentDefault.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEndorsementRequirementAssessmentDefault <- function(EndorsementRequirementAssessmentDefaultID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "EndorsementRequirementAssessmentDefault", objectId = EndorsementRequirementAssessmentDefaultID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EndorsementRequirementAssessmentDefault
	#'
	#' This function creates an EndorsementRequirementAssessmentDefault
	#' @param fieldNames The field values to give the created EndorsementRequirementAssessmentDefault. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created EndorsementRequirementAssessmentDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEndorsementRequirementAssessmentDefault <- function(EndorsementRequirementDefaultID = NULL, TestType = NULL, TestVersion = NULL, ClusterType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "EndorsementRequirementAssessmentDefault", body = list(DataObject = body), searchFields = append("EndorsementRequirementAssessmentDefaultID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EndorsementRequirementAssessmentDefault
	#'
	#' This function modifies an EndorsementRequirementAssessmentDefault
	#' @param fieldNames The field values to give the modified EndorsementRequirementAssessmentDefault. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified EndorsementRequirementAssessmentDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEndorsementRequirementAssessmentDefault <- function(EndorsementRequirementAssessmentDefaultID, EndorsementRequirementDefaultID = NULL, TestType = NULL, TestVersion = NULL, ClusterType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "EndorsementRequirementAssessmentDefault", objectId = EndorsementRequirementAssessmentDefaultID, body = list(DataObject = body), searchFields = append("EndorsementRequirementAssessmentDefaultID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EndorsementRequirementAssessmentScores
	#'
	#' This function returns a dataframe or json object of EndorsementRequirementAssessmentScores
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EndorsementRequirementAssessmentScores. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EndorsementRequirementAssessmentScores.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EndorsementRequirementAssessmentScore') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of EndorsementRequirementAssessmentScores
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEndorsementRequirementAssessmentScores <- function(searchConditionsList = NULL, EndorsementRequirementAssessmentScoreID = F, ScoreType = F, ScoreLocation = F, PassingScore = F, PassingScoreLow = F, PassingScoreHigh = F, EndorsementRequirementAssessmentScoreDefaultID = F, EndorsementRequirementAssessmentClusterID = F, AssessmentScoreColumn = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "EndorsementRequirementAssessmentScore", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EndorsementRequirementAssessmentScore
	#'
	#' This function returns a dataframe or json object of an EndorsementRequirementAssessmentScore
	#' @param EndorsementRequirementAssessmentScoreID The ID of the EndorsementRequirementAssessmentScore to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EndorsementRequirementAssessmentScore. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EndorsementRequirementAssessmentScore.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EndorsementRequirementAssessmentScore') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of EndorsementRequirementAssessmentScore
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEndorsementRequirementAssessmentScore <- function(EndorsementRequirementAssessmentScoreID, ScoreType = F, ScoreLocation = F, PassingScore = F, PassingScoreLow = F, PassingScoreHigh = F, EndorsementRequirementAssessmentScoreDefaultID = F, EndorsementRequirementAssessmentClusterID = F, AssessmentScoreColumn = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EndorsementRequirementAssessmentScoreID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "EndorsementRequirementAssessmentScore", objectId = EndorsementRequirementAssessmentScoreID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EndorsementRequirementAssessmentScore
	#'
	#' This function deletes an EndorsementRequirementAssessmentScore
	#' @param EndorsementRequirementAssessmentScoreID The ID of the EndorsementRequirementAssessmentScore to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The EndorsementRequirementAssessmentScoreID of the deleted EndorsementRequirementAssessmentScore.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEndorsementRequirementAssessmentScore <- function(EndorsementRequirementAssessmentScoreID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "EndorsementRequirementAssessmentScore", objectId = EndorsementRequirementAssessmentScoreID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EndorsementRequirementAssessmentScore
	#'
	#' This function creates an EndorsementRequirementAssessmentScore
	#' @param fieldNames The field values to give the created EndorsementRequirementAssessmentScore. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created EndorsementRequirementAssessmentScore
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEndorsementRequirementAssessmentScore <- function(ScoreType = NULL, ScoreLocation = NULL, PassingScore = NULL, PassingScoreLow = NULL, PassingScoreHigh = NULL, EndorsementRequirementAssessmentScoreDefaultID = NULL, EndorsementRequirementAssessmentClusterID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "EndorsementRequirementAssessmentScore", body = list(DataObject = body), searchFields = append("EndorsementRequirementAssessmentScoreID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EndorsementRequirementAssessmentScore
	#'
	#' This function modifies an EndorsementRequirementAssessmentScore
	#' @param fieldNames The field values to give the modified EndorsementRequirementAssessmentScore. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified EndorsementRequirementAssessmentScore
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEndorsementRequirementAssessmentScore <- function(EndorsementRequirementAssessmentScoreID, ScoreType = NULL, ScoreLocation = NULL, PassingScore = NULL, PassingScoreLow = NULL, PassingScoreHigh = NULL, EndorsementRequirementAssessmentScoreDefaultID = NULL, EndorsementRequirementAssessmentClusterID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "EndorsementRequirementAssessmentScore", objectId = EndorsementRequirementAssessmentScoreID, body = list(DataObject = body), searchFields = append("EndorsementRequirementAssessmentScoreID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EndorsementRequirementAssessmentScoreDefaults
	#'
	#' This function returns a dataframe or json object of EndorsementRequirementAssessmentScoreDefaults
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EndorsementRequirementAssessmentScoreDefaults. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EndorsementRequirementAssessmentScoreDefaults.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EndorsementRequirementAssessmentScoreDefault') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of EndorsementRequirementAssessmentScoreDefaults
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEndorsementRequirementAssessmentScoreDefaults <- function(searchConditionsList = NULL, EndorsementRequirementAssessmentScoreDefaultID = F, ScoreType = F, ScoreLocation = F, PassingScore = F, PassingScoreLow = F, PassingScoreHigh = F, EndorsementRequirementAssessmentClusterDefaultID = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "EndorsementRequirementAssessmentScoreDefault", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EndorsementRequirementAssessmentScoreDefault
	#'
	#' This function returns a dataframe or json object of an EndorsementRequirementAssessmentScoreDefault
	#' @param EndorsementRequirementAssessmentScoreDefaultID The ID of the EndorsementRequirementAssessmentScoreDefault to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EndorsementRequirementAssessmentScoreDefault. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EndorsementRequirementAssessmentScoreDefault.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EndorsementRequirementAssessmentScoreDefault') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of EndorsementRequirementAssessmentScoreDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEndorsementRequirementAssessmentScoreDefault <- function(EndorsementRequirementAssessmentScoreDefaultID, ScoreType = F, ScoreLocation = F, PassingScore = F, PassingScoreLow = F, PassingScoreHigh = F, EndorsementRequirementAssessmentClusterDefaultID = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EndorsementRequirementAssessmentScoreDefaultID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "EndorsementRequirementAssessmentScoreDefault", objectId = EndorsementRequirementAssessmentScoreDefaultID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EndorsementRequirementAssessmentScoreDefault
	#'
	#' This function deletes an EndorsementRequirementAssessmentScoreDefault
	#' @param EndorsementRequirementAssessmentScoreDefaultID The ID of the EndorsementRequirementAssessmentScoreDefault to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The EndorsementRequirementAssessmentScoreDefaultID of the deleted EndorsementRequirementAssessmentScoreDefault.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEndorsementRequirementAssessmentScoreDefault <- function(EndorsementRequirementAssessmentScoreDefaultID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "EndorsementRequirementAssessmentScoreDefault", objectId = EndorsementRequirementAssessmentScoreDefaultID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EndorsementRequirementAssessmentScoreDefault
	#'
	#' This function creates an EndorsementRequirementAssessmentScoreDefault
	#' @param fieldNames The field values to give the created EndorsementRequirementAssessmentScoreDefault. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created EndorsementRequirementAssessmentScoreDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEndorsementRequirementAssessmentScoreDefault <- function(ScoreType = NULL, ScoreLocation = NULL, PassingScore = NULL, PassingScoreLow = NULL, PassingScoreHigh = NULL, EndorsementRequirementAssessmentClusterDefaultID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "EndorsementRequirementAssessmentScoreDefault", body = list(DataObject = body), searchFields = append("EndorsementRequirementAssessmentScoreDefaultID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EndorsementRequirementAssessmentScoreDefault
	#'
	#' This function modifies an EndorsementRequirementAssessmentScoreDefault
	#' @param fieldNames The field values to give the modified EndorsementRequirementAssessmentScoreDefault. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified EndorsementRequirementAssessmentScoreDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEndorsementRequirementAssessmentScoreDefault <- function(EndorsementRequirementAssessmentScoreDefaultID, ScoreType = NULL, ScoreLocation = NULL, PassingScore = NULL, PassingScoreLow = NULL, PassingScoreHigh = NULL, EndorsementRequirementAssessmentClusterDefaultID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "EndorsementRequirementAssessmentScoreDefault", objectId = EndorsementRequirementAssessmentScoreDefaultID, body = list(DataObject = body), searchFields = append("EndorsementRequirementAssessmentScoreDefaultID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentEndorsementRequirementAssessments
	#'
	#' This function returns a dataframe or json object of StudentEndorsementRequirementAssessments
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentEndorsementRequirementAssessments. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentEndorsementRequirementAssessments.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentEndorsementRequirementAssessment') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of StudentEndorsementRequirementAssessments
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentEndorsementRequirementAssessments <- function(searchConditionsList = NULL, StudentEndorsementRequirementAssessmentID = F, StudentEndorsementRequirementID = F, EndorsementRequirementAssessmentID = F, IsComplete = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "StudentEndorsementRequirementAssessment", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentEndorsementRequirementAssessment
	#'
	#' This function returns a dataframe or json object of a StudentEndorsementRequirementAssessment
	#' @param StudentEndorsementRequirementAssessmentID The ID of the StudentEndorsementRequirementAssessment to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentEndorsementRequirementAssessment. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentEndorsementRequirementAssessment.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentEndorsementRequirementAssessment') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of StudentEndorsementRequirementAssessment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentEndorsementRequirementAssessment <- function(StudentEndorsementRequirementAssessmentID, StudentEndorsementRequirementID = F, EndorsementRequirementAssessmentID = F, IsComplete = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentEndorsementRequirementAssessmentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "StudentEndorsementRequirementAssessment", objectId = StudentEndorsementRequirementAssessmentID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentEndorsementRequirementAssessment
	#'
	#' This function deletes a StudentEndorsementRequirementAssessment
	#' @param StudentEndorsementRequirementAssessmentID The ID of the StudentEndorsementRequirementAssessment to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The StudentEndorsementRequirementAssessmentID of the deleted StudentEndorsementRequirementAssessment.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentEndorsementRequirementAssessment <- function(StudentEndorsementRequirementAssessmentID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "StudentEndorsementRequirementAssessment", objectId = StudentEndorsementRequirementAssessmentID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentEndorsementRequirementAssessment
	#'
	#' This function creates a StudentEndorsementRequirementAssessment
	#' @param fieldNames The field values to give the created StudentEndorsementRequirementAssessment. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created StudentEndorsementRequirementAssessment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentEndorsementRequirementAssessment <- function(StudentEndorsementRequirementID = NULL, EndorsementRequirementAssessmentID = NULL, IsComplete = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "StudentEndorsementRequirementAssessment", body = list(DataObject = body), searchFields = append("StudentEndorsementRequirementAssessmentID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentEndorsementRequirementAssessment
	#'
	#' This function modifies a StudentEndorsementRequirementAssessment
	#' @param fieldNames The field values to give the modified StudentEndorsementRequirementAssessment. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified StudentEndorsementRequirementAssessment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentEndorsementRequirementAssessment <- function(StudentEndorsementRequirementAssessmentID, StudentEndorsementRequirementID = NULL, EndorsementRequirementAssessmentID = NULL, IsComplete = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "StudentEndorsementRequirementAssessment", objectId = StudentEndorsementRequirementAssessmentID, body = list(DataObject = body), searchFields = append("StudentEndorsementRequirementAssessmentID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentEndorsementRequirementAssessmentScores
	#'
	#' This function returns a dataframe or json object of StudentEndorsementRequirementAssessmentScores
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentEndorsementRequirementAssessmentScores. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentEndorsementRequirementAssessmentScores.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentEndorsementRequirementAssessmentScore') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of StudentEndorsementRequirementAssessmentScores
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentEndorsementRequirementAssessmentScores <- function(searchConditionsList = NULL, StudentEndorsementRequirementAssessmentScoreID = F, EndorsementRequirementAssessmentScoreID = F, StudentEndorsementRequirementAssessmentID = F, AssessmentScore = F, IsPassingScore = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "StudentEndorsementRequirementAssessmentScore", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentEndorsementRequirementAssessmentScore
	#'
	#' This function returns a dataframe or json object of a StudentEndorsementRequirementAssessmentScore
	#' @param StudentEndorsementRequirementAssessmentScoreID The ID of the StudentEndorsementRequirementAssessmentScore to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentEndorsementRequirementAssessmentScore. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentEndorsementRequirementAssessmentScore.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentEndorsementRequirementAssessmentScore') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of StudentEndorsementRequirementAssessmentScore
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentEndorsementRequirementAssessmentScore <- function(StudentEndorsementRequirementAssessmentScoreID, EndorsementRequirementAssessmentScoreID = F, StudentEndorsementRequirementAssessmentID = F, AssessmentScore = F, IsPassingScore = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentEndorsementRequirementAssessmentScoreID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "StudentEndorsementRequirementAssessmentScore", objectId = StudentEndorsementRequirementAssessmentScoreID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentEndorsementRequirementAssessmentScore
	#'
	#' This function deletes a StudentEndorsementRequirementAssessmentScore
	#' @param StudentEndorsementRequirementAssessmentScoreID The ID of the StudentEndorsementRequirementAssessmentScore to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The StudentEndorsementRequirementAssessmentScoreID of the deleted StudentEndorsementRequirementAssessmentScore.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentEndorsementRequirementAssessmentScore <- function(StudentEndorsementRequirementAssessmentScoreID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "StudentEndorsementRequirementAssessmentScore", objectId = StudentEndorsementRequirementAssessmentScoreID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentEndorsementRequirementAssessmentScore
	#'
	#' This function creates a StudentEndorsementRequirementAssessmentScore
	#' @param fieldNames The field values to give the created StudentEndorsementRequirementAssessmentScore. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created StudentEndorsementRequirementAssessmentScore
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentEndorsementRequirementAssessmentScore <- function(EndorsementRequirementAssessmentScoreID = NULL, StudentEndorsementRequirementAssessmentID = NULL, AssessmentScore = NULL, IsPassingScore = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "StudentEndorsementRequirementAssessmentScore", body = list(DataObject = body), searchFields = append("StudentEndorsementRequirementAssessmentScoreID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentEndorsementRequirementAssessmentScore
	#'
	#' This function modifies a StudentEndorsementRequirementAssessmentScore
	#' @param fieldNames The field values to give the modified StudentEndorsementRequirementAssessmentScore. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified StudentEndorsementRequirementAssessmentScore
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentEndorsementRequirementAssessmentScore <- function(StudentEndorsementRequirementAssessmentScoreID, EndorsementRequirementAssessmentScoreID = NULL, StudentEndorsementRequirementAssessmentID = NULL, AssessmentScore = NULL, IsPassingScore = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "StudentEndorsementRequirementAssessmentScore", objectId = StudentEndorsementRequirementAssessmentScoreID, body = list(DataObject = body), searchFields = append("StudentEndorsementRequirementAssessmentScoreID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentPlanThreadLocks
	#'
	#' This function returns a dataframe or json object of StudentPlanThreadLocks
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentPlanThreadLocks. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentPlanThreadLocks.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentPlanThreadLock') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of StudentPlanThreadLocks
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentPlanThreadLocks <- function(searchConditionsList = NULL, StudentPlanThreadLockID = F, StudentPlanID = F, DistrictID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "StudentPlanThreadLock", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentPlanThreadLock
	#'
	#' This function returns a dataframe or json object of a StudentPlanThreadLock
	#' @param StudentPlanThreadLockID The ID of the StudentPlanThreadLock to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentPlanThreadLock. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentPlanThreadLock.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentPlanThreadLock') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of StudentPlanThreadLock
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentPlanThreadLock <- function(StudentPlanThreadLockID, StudentPlanID = F, DistrictID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentPlanThreadLockID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "StudentPlanThreadLock", objectId = StudentPlanThreadLockID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentPlanThreadLock
	#'
	#' This function deletes a StudentPlanThreadLock
	#' @param StudentPlanThreadLockID The ID of the StudentPlanThreadLock to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The StudentPlanThreadLockID of the deleted StudentPlanThreadLock.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentPlanThreadLock <- function(StudentPlanThreadLockID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "StudentPlanThreadLock", objectId = StudentPlanThreadLockID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentPlanThreadLock
	#'
	#' This function creates a StudentPlanThreadLock
	#' @param fieldNames The field values to give the created StudentPlanThreadLock. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created StudentPlanThreadLock
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentPlanThreadLock <- function(StudentPlanID = NULL, DistrictID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "StudentPlanThreadLock", body = list(DataObject = body), searchFields = append("StudentPlanThreadLockID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentPlanThreadLock
	#'
	#' This function modifies a StudentPlanThreadLock
	#' @param fieldNames The field values to give the modified StudentPlanThreadLock. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified StudentPlanThreadLock
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentPlanThreadLock <- function(StudentPlanThreadLockID, StudentPlanID = NULL, DistrictID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "StudentPlanThreadLock", objectId = StudentPlanThreadLockID, body = list(DataObject = body), searchFields = append("StudentPlanThreadLockID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List QueuedStudentEndorsementCalculations
	#'
	#' This function returns a dataframe or json object of QueuedStudentEndorsementCalculations
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given QueuedStudentEndorsementCalculations. Defaults to FALSE for all return fields which, for convenience, returns all fields for the QueuedStudentEndorsementCalculations.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('QueuedStudentEndorsementCalculation') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of QueuedStudentEndorsementCalculations
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listQueuedStudentEndorsementCalculations <- function(searchConditionsList = NULL, QueuedStudentEndorsementCalculationID = F, StudentID = F, DistrictID = F, StatusCode = F, Status = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "QueuedStudentEndorsementCalculation", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a QueuedStudentEndorsementCalculation
	#'
	#' This function returns a dataframe or json object of a QueuedStudentEndorsementCalculation
	#' @param QueuedStudentEndorsementCalculationID The ID of the QueuedStudentEndorsementCalculation to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given QueuedStudentEndorsementCalculation. Defaults to FALSE for all return fields which, for convenience, returns all fields for the QueuedStudentEndorsementCalculation.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('QueuedStudentEndorsementCalculation') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of QueuedStudentEndorsementCalculation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getQueuedStudentEndorsementCalculation <- function(QueuedStudentEndorsementCalculationID, StudentID = F, DistrictID = F, StatusCode = F, Status = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "QueuedStudentEndorsementCalculationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "QueuedStudentEndorsementCalculation", objectId = QueuedStudentEndorsementCalculationID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a QueuedStudentEndorsementCalculation
	#'
	#' This function deletes a QueuedStudentEndorsementCalculation
	#' @param QueuedStudentEndorsementCalculationID The ID of the QueuedStudentEndorsementCalculation to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The QueuedStudentEndorsementCalculationID of the deleted QueuedStudentEndorsementCalculation.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteQueuedStudentEndorsementCalculation <- function(QueuedStudentEndorsementCalculationID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "QueuedStudentEndorsementCalculation", objectId = QueuedStudentEndorsementCalculationID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a QueuedStudentEndorsementCalculation
	#'
	#' This function creates a QueuedStudentEndorsementCalculation
	#' @param fieldNames The field values to give the created QueuedStudentEndorsementCalculation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created QueuedStudentEndorsementCalculation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createQueuedStudentEndorsementCalculation <- function(StudentID = NULL, DistrictID = NULL, Status = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "QueuedStudentEndorsementCalculation", body = list(DataObject = body), searchFields = append("QueuedStudentEndorsementCalculationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a QueuedStudentEndorsementCalculation
	#'
	#' This function modifies a QueuedStudentEndorsementCalculation
	#' @param fieldNames The field values to give the modified QueuedStudentEndorsementCalculation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified QueuedStudentEndorsementCalculation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyQueuedStudentEndorsementCalculation <- function(QueuedStudentEndorsementCalculationID, StudentID = NULL, DistrictID = NULL, Status = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "QueuedStudentEndorsementCalculation", objectId = QueuedStudentEndorsementCalculationID, body = list(DataObject = body), searchFields = append("QueuedStudentEndorsementCalculationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentCareerPlanSummaries
	#'
	#' This function returns a dataframe or json object of StudentCareerPlanSummaries
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentCareerPlanSummaries. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentCareerPlanSummaries.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentCareerPlanSummary') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of StudentCareerPlanSummaries
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentCareerPlanSummaries <- function(searchConditionsList = NULL, StudentCareerPlanSummaryID = F, StudentID = F, DistrictID = F, IsSignedByStudent = F, IsSignedByGuardian = F, StudentSignedTime = F, GuardianSignedTime = F, NameIDGuardianSignedBy = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "StudentCareerPlanSummary", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentCareerPlanSummary
	#'
	#' This function returns a dataframe or json object of a StudentCareerPlanSummary
	#' @param StudentCareerPlanSummaryID The ID of the StudentCareerPlanSummary to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentCareerPlanSummary. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentCareerPlanSummary.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentCareerPlanSummary') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of StudentCareerPlanSummary
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentCareerPlanSummary <- function(StudentCareerPlanSummaryID, StudentID = F, DistrictID = F, IsSignedByStudent = F, IsSignedByGuardian = F, StudentSignedTime = F, GuardianSignedTime = F, NameIDGuardianSignedBy = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentCareerPlanSummaryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "StudentCareerPlanSummary", objectId = StudentCareerPlanSummaryID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentCareerPlanSummary
	#'
	#' This function deletes a StudentCareerPlanSummary
	#' @param StudentCareerPlanSummaryID The ID of the StudentCareerPlanSummary to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The StudentCareerPlanSummaryID of the deleted StudentCareerPlanSummary.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentCareerPlanSummary <- function(StudentCareerPlanSummaryID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "StudentCareerPlanSummary", objectId = StudentCareerPlanSummaryID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentCareerPlanSummary
	#'
	#' This function creates a StudentCareerPlanSummary
	#' @param fieldNames The field values to give the created StudentCareerPlanSummary. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created StudentCareerPlanSummary
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentCareerPlanSummary <- function(StudentID = NULL, DistrictID = NULL, IsSignedByStudent = NULL, IsSignedByGuardian = NULL, StudentSignedTime = NULL, GuardianSignedTime = NULL, NameIDGuardianSignedBy = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "StudentCareerPlanSummary", body = list(DataObject = body), searchFields = append("StudentCareerPlanSummaryID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentCareerPlanSummary
	#'
	#' This function modifies a StudentCareerPlanSummary
	#' @param fieldNames The field values to give the modified StudentCareerPlanSummary. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified StudentCareerPlanSummary
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentCareerPlanSummary <- function(StudentCareerPlanSummaryID, StudentID = NULL, DistrictID = NULL, IsSignedByStudent = NULL, IsSignedByGuardian = NULL, StudentSignedTime = NULL, GuardianSignedTime = NULL, NameIDGuardianSignedBy = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "StudentCareerPlanSummary", objectId = StudentCareerPlanSummaryID, body = list(DataObject = body), searchFields = append("StudentCareerPlanSummaryID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempStudentEndorsementErrors
	#'
	#' This function returns a dataframe or json object of TempStudentEndorsementErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentEndorsementErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentEndorsementErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentEndorsementError') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of TempStudentEndorsementErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStudentEndorsementErrors <- function(searchConditionsList = NULL, TempStudentEndorsementErrorID = F, FirstName = F, LastName = F, MiddleName = F, BirthDate = F, Gender = F, StudentNumber = F, EndorsementCode = F, StateID = F, EndorsementOptionCode = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "TempStudentEndorsementError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempStudentEndorsementError
	#'
	#' This function returns a dataframe or json object of a TempStudentEndorsementError
	#' @param TempStudentEndorsementErrorID The ID of the TempStudentEndorsementError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentEndorsementError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentEndorsementError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentEndorsementError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of TempStudentEndorsementError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempStudentEndorsementError <- function(TempStudentEndorsementErrorID, FirstName = F, LastName = F, MiddleName = F, BirthDate = F, Gender = F, StudentNumber = F, EndorsementCode = F, StateID = F, EndorsementOptionCode = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempStudentEndorsementErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "TempStudentEndorsementError", objectId = TempStudentEndorsementErrorID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempStudentEndorsementError
	#'
	#' This function deletes a TempStudentEndorsementError
	#' @param TempStudentEndorsementErrorID The ID of the TempStudentEndorsementError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The TempStudentEndorsementErrorID of the deleted TempStudentEndorsementError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempStudentEndorsementError <- function(TempStudentEndorsementErrorID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "TempStudentEndorsementError", objectId = TempStudentEndorsementErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempStudentEndorsementError
	#'
	#' This function creates a TempStudentEndorsementError
	#' @param fieldNames The field values to give the created TempStudentEndorsementError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created TempStudentEndorsementError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempStudentEndorsementError <- function(FirstName = NULL, LastName = NULL, MiddleName = NULL, BirthDate = NULL, Gender = NULL, StudentNumber = NULL, EndorsementCode = NULL, StateID = NULL, EndorsementOptionCode = NULL, Message = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "TempStudentEndorsementError", body = list(DataObject = body), searchFields = append("TempStudentEndorsementErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempStudentEndorsementError
	#'
	#' This function modifies a TempStudentEndorsementError
	#' @param fieldNames The field values to give the modified TempStudentEndorsementError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified TempStudentEndorsementError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempStudentEndorsementError <- function(TempStudentEndorsementErrorID, FirstName = NULL, LastName = NULL, MiddleName = NULL, BirthDate = NULL, Gender = NULL, StudentNumber = NULL, EndorsementCode = NULL, StateID = NULL, EndorsementOptionCode = NULL, Message = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "TempStudentEndorsementError", objectId = TempStudentEndorsementErrorID, body = list(DataObject = body), searchFields = append("TempStudentEndorsementErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempStudentEndorsements
	#'
	#' This function returns a dataframe or json object of TempStudentEndorsements
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentEndorsements. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentEndorsements.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentEndorsement') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A list of TempStudentEndorsements
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStudentEndorsements <- function(searchConditionsList = NULL, TempStudentEndorsementID = F, StudentID = F, StudentNumber = F, FirstName = F, LastName = F, MiddleName = F, BirthDate = F, Gender = F, DateReceived = F, IsReceived = F, EndorsementCode = F, EndorsementID = F, StudentEndorsementID = F, EndorsementOptionCode = F, EndorsementOptionID = F, StudentEndorsementOptionID = F, Action = F, StateID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "GraduationRequirements", objectName = "TempStudentEndorsement", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempStudentEndorsement
	#'
	#' This function returns a dataframe or json object of a TempStudentEndorsement
	#' @param TempStudentEndorsementID The ID of the TempStudentEndorsement to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentEndorsement. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentEndorsement.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentEndorsement') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A dataframe or of TempStudentEndorsement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempStudentEndorsement <- function(TempStudentEndorsementID, StudentID = F, StudentNumber = F, FirstName = F, LastName = F, MiddleName = F, BirthDate = F, Gender = F, DateReceived = F, IsReceived = F, EndorsementCode = F, EndorsementID = F, StudentEndorsementID = F, EndorsementOptionCode = F, EndorsementOptionID = F, StudentEndorsementOptionID = F, Action = F, StateID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempStudentEndorsementID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "GraduationRequirements", objectName = "TempStudentEndorsement", objectId = TempStudentEndorsementID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempStudentEndorsement
	#'
	#' This function deletes a TempStudentEndorsement
	#' @param TempStudentEndorsementID The ID of the TempStudentEndorsement to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The TempStudentEndorsementID of the deleted TempStudentEndorsement.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempStudentEndorsement <- function(TempStudentEndorsementID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "GraduationRequirements", objectName = "TempStudentEndorsement", objectId = TempStudentEndorsementID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempStudentEndorsement
	#'
	#' This function creates a TempStudentEndorsement
	#' @param fieldNames The field values to give the created TempStudentEndorsement. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return A newly created TempStudentEndorsement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempStudentEndorsement <- function(StudentID = NULL, StudentNumber = NULL, FirstName = NULL, LastName = NULL, MiddleName = NULL, BirthDate = NULL, Gender = NULL, DateReceived = NULL, IsReceived = NULL, EndorsementCode = NULL, EndorsementID = NULL, StudentEndorsementID = NULL, EndorsementOptionCode = NULL, EndorsementOptionID = NULL, StudentEndorsementOptionID = NULL, Action = NULL, StateID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "GraduationRequirements", objectName = "TempStudentEndorsement", body = list(DataObject = body), searchFields = append("TempStudentEndorsementID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempStudentEndorsement
	#'
	#' This function modifies a TempStudentEndorsement
	#' @param fieldNames The field values to give the modified TempStudentEndorsement. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept GraduationRequirements
	#' @return The modified TempStudentEndorsement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempStudentEndorsement <- function(TempStudentEndorsementID, StudentID = NULL, StudentNumber = NULL, FirstName = NULL, LastName = NULL, MiddleName = NULL, BirthDate = NULL, Gender = NULL, DateReceived = NULL, IsReceived = NULL, EndorsementCode = NULL, EndorsementID = NULL, StudentEndorsementID = NULL, EndorsementOptionCode = NULL, EndorsementOptionID = NULL, StudentEndorsementOptionID = NULL, Action = NULL, StateID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "GraduationRequirements", objectName = "TempStudentEndorsement", objectId = TempStudentEndorsementID, body = list(DataObject = body), searchFields = append("TempStudentEndorsementID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}
