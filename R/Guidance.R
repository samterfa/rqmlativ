
	#' List StudentOfficeVisits
	#'
	#' This function returns a dataframe or json object of StudentOfficeVisits
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentOfficeVisits. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentOfficeVisits.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentOfficeVisit') to get more field paths.
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
	#' @concept Guidance
	#' @return A list of StudentOfficeVisits
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentOfficeVisits <- function(searchConditionsList = NULL, CreatedTime = F, DisplayStatus = F, DocumentationIsComplete = F, EntityID = F, HasBeenReleased = F, IsStudentOfficeVisitToday = F, ModifiedTime = F, OfficeVisitCommentID = F, SchoolID = F, SchoolYearID = F, StudentID = F, StudentOfficeVisitID = F, StudentOfficeVisitReasonsListDisplay = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Guidance", objectName = "StudentOfficeVisit", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentOfficeVisit
	#'
	#' This function returns a dataframe or json object of a StudentOfficeVisit
	#' @param StudentOfficeVisitID The ID of the StudentOfficeVisit to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentOfficeVisit. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentOfficeVisit.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentOfficeVisit') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Guidance
	#' @return A dataframe or of StudentOfficeVisit
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentOfficeVisit <- function(StudentOfficeVisitID, CreatedTime = F, DisplayStatus = F, DocumentationIsComplete = F, EntityID = F, HasBeenReleased = F, IsStudentOfficeVisitToday = F, ModifiedTime = F, OfficeVisitCommentID = F, SchoolID = F, SchoolYearID = F, StudentID = F, StudentOfficeVisitReasonsListDisplay = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentOfficeVisitID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Guidance", objectName = "StudentOfficeVisit", objectId = StudentOfficeVisitID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentOfficeVisit
	#'
	#' This function deletes a StudentOfficeVisit
	#' @param StudentOfficeVisitID The ID of the StudentOfficeVisit to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Guidance
	#' @return The StudentOfficeVisitID of the deleted StudentOfficeVisit.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentOfficeVisit <- function(StudentOfficeVisitID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Guidance", objectName = "StudentOfficeVisit", objectId = StudentOfficeVisitID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentOfficeVisit
	#'
	#' This function creates a StudentOfficeVisit
	#' @param fieldNames The field values to give the created StudentOfficeVisit. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Guidance
	#' @return A newly created StudentOfficeVisit
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentOfficeVisit <- function(DocumentationIsComplete = NULL, EntityID = NULL, OfficeVisitCommentID = NULL, SchoolID = NULL, SchoolYearID = NULL, StudentID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Guidance", objectName = "StudentOfficeVisit", body = list(DataObject = body), searchFields = append("StudentOfficeVisitID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentOfficeVisit
	#'
	#' This function modifies a StudentOfficeVisit
	#' @param fieldNames The field values to give the modified StudentOfficeVisit. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Guidance
	#' @return The modified StudentOfficeVisit
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentOfficeVisit <- function(StudentOfficeVisitID, DocumentationIsComplete = NULL, EntityID = NULL, OfficeVisitCommentID = NULL, SchoolID = NULL, SchoolYearID = NULL, StudentID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Guidance", objectName = "StudentOfficeVisit", objectId = StudentOfficeVisitID, body = list(DataObject = body), searchFields = append("StudentOfficeVisitID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentOfficeVisitNotes
	#'
	#' This function returns a dataframe or json object of StudentOfficeVisitNotes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentOfficeVisitNotes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentOfficeVisitNotes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentOfficeVisitNote') to get more field paths.
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
	#' @concept Guidance
	#' @return A list of StudentOfficeVisitNotes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentOfficeVisitNotes <- function(searchConditionsList = NULL, CreatedTime = F, ModifiedTime = F, Note = F, StudentOfficeVisitID = F, StudentOfficeVisitNoteID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Guidance", objectName = "StudentOfficeVisitNote", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentOfficeVisitNote
	#'
	#' This function returns a dataframe or json object of a StudentOfficeVisitNote
	#' @param StudentOfficeVisitNoteID The ID of the StudentOfficeVisitNote to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentOfficeVisitNote. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentOfficeVisitNote.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentOfficeVisitNote') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Guidance
	#' @return A dataframe or of StudentOfficeVisitNote
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentOfficeVisitNote <- function(StudentOfficeVisitNoteID, CreatedTime = F, ModifiedTime = F, Note = F, StudentOfficeVisitID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentOfficeVisitNoteID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Guidance", objectName = "StudentOfficeVisitNote", objectId = StudentOfficeVisitNoteID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentOfficeVisitNote
	#'
	#' This function deletes a StudentOfficeVisitNote
	#' @param StudentOfficeVisitNoteID The ID of the StudentOfficeVisitNote to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Guidance
	#' @return The StudentOfficeVisitNoteID of the deleted StudentOfficeVisitNote.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentOfficeVisitNote <- function(StudentOfficeVisitNoteID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Guidance", objectName = "StudentOfficeVisitNote", objectId = StudentOfficeVisitNoteID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentOfficeVisitNote
	#'
	#' This function creates a StudentOfficeVisitNote
	#' @param fieldNames The field values to give the created StudentOfficeVisitNote. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Guidance
	#' @return A newly created StudentOfficeVisitNote
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentOfficeVisitNote <- function(Note = NULL, StudentOfficeVisitID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Guidance", objectName = "StudentOfficeVisitNote", body = list(DataObject = body), searchFields = append("StudentOfficeVisitNoteID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentOfficeVisitNote
	#'
	#' This function modifies a StudentOfficeVisitNote
	#' @param fieldNames The field values to give the modified StudentOfficeVisitNote. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Guidance
	#' @return The modified StudentOfficeVisitNote
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentOfficeVisitNote <- function(StudentOfficeVisitNoteID, Note = NULL, StudentOfficeVisitID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Guidance", objectName = "StudentOfficeVisitNote", objectId = StudentOfficeVisitNoteID, body = list(DataObject = body), searchFields = append("StudentOfficeVisitNoteID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentOfficeVisitNotifications
	#'
	#' This function returns a dataframe or json object of StudentOfficeVisitNotifications
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentOfficeVisitNotifications. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentOfficeVisitNotifications.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentOfficeVisitNotification') to get more field paths.
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
	#' @concept Guidance
	#' @return A list of StudentOfficeVisitNotifications
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentOfficeVisitNotifications <- function(searchConditionsList = NULL, CreatedTime = F, ModifiedTime = F, NameID = F, Note = F, NotificationMethodID = F, NotificationTime = F, OfficeVisitGuardianResponseID = F, StudentOfficeVisitID = F, StudentOfficeVisitNotificationID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Guidance", objectName = "StudentOfficeVisitNotification", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentOfficeVisitNotification
	#'
	#' This function returns a dataframe or json object of a StudentOfficeVisitNotification
	#' @param StudentOfficeVisitNotificationID The ID of the StudentOfficeVisitNotification to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentOfficeVisitNotification. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentOfficeVisitNotification.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentOfficeVisitNotification') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Guidance
	#' @return A dataframe or of StudentOfficeVisitNotification
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentOfficeVisitNotification <- function(StudentOfficeVisitNotificationID, CreatedTime = F, ModifiedTime = F, NameID = F, Note = F, NotificationMethodID = F, NotificationTime = F, OfficeVisitGuardianResponseID = F, StudentOfficeVisitID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentOfficeVisitNotificationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Guidance", objectName = "StudentOfficeVisitNotification", objectId = StudentOfficeVisitNotificationID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentOfficeVisitNotification
	#'
	#' This function deletes a StudentOfficeVisitNotification
	#' @param StudentOfficeVisitNotificationID The ID of the StudentOfficeVisitNotification to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Guidance
	#' @return The StudentOfficeVisitNotificationID of the deleted StudentOfficeVisitNotification.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentOfficeVisitNotification <- function(StudentOfficeVisitNotificationID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Guidance", objectName = "StudentOfficeVisitNotification", objectId = StudentOfficeVisitNotificationID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentOfficeVisitNotification
	#'
	#' This function creates a StudentOfficeVisitNotification
	#' @param fieldNames The field values to give the created StudentOfficeVisitNotification. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Guidance
	#' @return A newly created StudentOfficeVisitNotification
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentOfficeVisitNotification <- function(NameID = NULL, Note = NULL, NotificationMethodID = NULL, NotificationTime = NULL, OfficeVisitGuardianResponseID = NULL, StudentOfficeVisitID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Guidance", objectName = "StudentOfficeVisitNotification", body = list(DataObject = body), searchFields = append("StudentOfficeVisitNotificationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentOfficeVisitNotification
	#'
	#' This function modifies a StudentOfficeVisitNotification
	#' @param fieldNames The field values to give the modified StudentOfficeVisitNotification. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Guidance
	#' @return The modified StudentOfficeVisitNotification
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentOfficeVisitNotification <- function(StudentOfficeVisitNotificationID, NameID = NULL, Note = NULL, NotificationMethodID = NULL, NotificationTime = NULL, OfficeVisitGuardianResponseID = NULL, StudentOfficeVisitID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Guidance", objectName = "StudentOfficeVisitNotification", objectId = StudentOfficeVisitNotificationID, body = list(DataObject = body), searchFields = append("StudentOfficeVisitNotificationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentOfficeVisitReasons
	#'
	#' This function returns a dataframe or json object of StudentOfficeVisitReasons
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentOfficeVisitReasons. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentOfficeVisitReasons.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentOfficeVisitReason') to get more field paths.
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
	#' @concept Guidance
	#' @return A list of StudentOfficeVisitReasons
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentOfficeVisitReasons <- function(searchConditionsList = NULL, CreatedTime = F, ModifiedTime = F, OfficeVisitReasonID = F, StudentOfficeVisitID = F, StudentOfficeVisitReasonID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Guidance", objectName = "StudentOfficeVisitReason", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentOfficeVisitReason
	#'
	#' This function returns a dataframe or json object of a StudentOfficeVisitReason
	#' @param StudentOfficeVisitReasonID The ID of the StudentOfficeVisitReason to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentOfficeVisitReason. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentOfficeVisitReason.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentOfficeVisitReason') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Guidance
	#' @return A dataframe or of StudentOfficeVisitReason
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentOfficeVisitReason <- function(StudentOfficeVisitReasonID, CreatedTime = F, ModifiedTime = F, OfficeVisitReasonID = F, StudentOfficeVisitID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentOfficeVisitReasonID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Guidance", objectName = "StudentOfficeVisitReason", objectId = StudentOfficeVisitReasonID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentOfficeVisitReason
	#'
	#' This function deletes a StudentOfficeVisitReason
	#' @param StudentOfficeVisitReasonID The ID of the StudentOfficeVisitReason to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Guidance
	#' @return The StudentOfficeVisitReasonID of the deleted StudentOfficeVisitReason.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentOfficeVisitReason <- function(StudentOfficeVisitReasonID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Guidance", objectName = "StudentOfficeVisitReason", objectId = StudentOfficeVisitReasonID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentOfficeVisitReason
	#'
	#' This function creates a StudentOfficeVisitReason
	#' @param fieldNames The field values to give the created StudentOfficeVisitReason. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Guidance
	#' @return A newly created StudentOfficeVisitReason
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentOfficeVisitReason <- function(OfficeVisitReasonID = NULL, StudentOfficeVisitID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Guidance", objectName = "StudentOfficeVisitReason", body = list(DataObject = body), searchFields = append("StudentOfficeVisitReasonID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentOfficeVisitReason
	#'
	#' This function modifies a StudentOfficeVisitReason
	#' @param fieldNames The field values to give the modified StudentOfficeVisitReason. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Guidance
	#' @return The modified StudentOfficeVisitReason
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentOfficeVisitReason <- function(StudentOfficeVisitReasonID, OfficeVisitReasonID = NULL, StudentOfficeVisitID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Guidance", objectName = "StudentOfficeVisitReason", objectId = StudentOfficeVisitReasonID, body = list(DataObject = body), searchFields = append("StudentOfficeVisitReasonID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentOfficeVisitTimeTransactions
	#'
	#' This function returns a dataframe or json object of StudentOfficeVisitTimeTransactions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentOfficeVisitTimeTransactions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentOfficeVisitTimeTransactions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentOfficeVisitTimeTransaction') to get more field paths.
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
	#' @concept Guidance
	#' @return A list of StudentOfficeVisitTimeTransactions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentOfficeVisitTimeTransactions <- function(searchConditionsList = NULL, CreatedTime = F, DisplayDuration = F, DisplayOrder = F, Duration = F, EndTime = F, ModifiedTime = F, Note = F, StartTime = F, Status = F, StudentOfficeVisitID = F, StudentOfficeVisitTimeTransactionID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Guidance", objectName = "StudentOfficeVisitTimeTransaction", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentOfficeVisitTimeTransaction
	#'
	#' This function returns a dataframe or json object of a StudentOfficeVisitTimeTransaction
	#' @param StudentOfficeVisitTimeTransactionID The ID of the StudentOfficeVisitTimeTransaction to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentOfficeVisitTimeTransaction. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentOfficeVisitTimeTransaction.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentOfficeVisitTimeTransaction') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Guidance
	#' @return A dataframe or of StudentOfficeVisitTimeTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentOfficeVisitTimeTransaction <- function(StudentOfficeVisitTimeTransactionID, CreatedTime = F, DisplayDuration = F, DisplayOrder = F, Duration = F, EndTime = F, ModifiedTime = F, Note = F, StartTime = F, Status = F, StudentOfficeVisitID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentOfficeVisitTimeTransactionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Guidance", objectName = "StudentOfficeVisitTimeTransaction", objectId = StudentOfficeVisitTimeTransactionID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentOfficeVisitTimeTransaction
	#'
	#' This function deletes a StudentOfficeVisitTimeTransaction
	#' @param StudentOfficeVisitTimeTransactionID The ID of the StudentOfficeVisitTimeTransaction to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Guidance
	#' @return The StudentOfficeVisitTimeTransactionID of the deleted StudentOfficeVisitTimeTransaction.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentOfficeVisitTimeTransaction <- function(StudentOfficeVisitTimeTransactionID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Guidance", objectName = "StudentOfficeVisitTimeTransaction", objectId = StudentOfficeVisitTimeTransactionID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentOfficeVisitTimeTransaction
	#'
	#' This function creates a StudentOfficeVisitTimeTransaction
	#' @param fieldNames The field values to give the created StudentOfficeVisitTimeTransaction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Guidance
	#' @return A newly created StudentOfficeVisitTimeTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentOfficeVisitTimeTransaction <- function(DisplayOrder = NULL, EndTime = NULL, Note = NULL, StartTime = NULL, Status = NULL, StudentOfficeVisitID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Guidance", objectName = "StudentOfficeVisitTimeTransaction", body = list(DataObject = body), searchFields = append("StudentOfficeVisitTimeTransactionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentOfficeVisitTimeTransaction
	#'
	#' This function modifies a StudentOfficeVisitTimeTransaction
	#' @param fieldNames The field values to give the modified StudentOfficeVisitTimeTransaction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Guidance
	#' @return The modified StudentOfficeVisitTimeTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentOfficeVisitTimeTransaction <- function(StudentOfficeVisitTimeTransactionID, DisplayOrder = NULL, EndTime = NULL, Note = NULL, StartTime = NULL, Status = NULL, StudentOfficeVisitID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Guidance", objectName = "StudentOfficeVisitTimeTransaction", objectId = StudentOfficeVisitTimeTransactionID, body = list(DataObject = body), searchFields = append("StudentOfficeVisitTimeTransactionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List GuidanceOfficeVisitComments
	#'
	#' This function returns a dataframe or json object of GuidanceOfficeVisitComments
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given GuidanceOfficeVisitComments. Defaults to FALSE for all return fields which, for convenience, returns all fields for the GuidanceOfficeVisitComments.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('GuidanceOfficeVisitComment') to get more field paths.
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
	#' @concept Guidance
	#' @return A list of GuidanceOfficeVisitComments
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listGuidanceOfficeVisitComments <- function(searchConditionsList = NULL, Code = F, CodeDescription = F, CreatedTime = F, Description = F, DistrictGroupKey = F, DistrictID = F, IsActive = F, ModifiedTime = F, OfficeVisitCommentID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Guidance", objectName = "OfficeVisitComment", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a GuidanceOfficeVisitComment
	#'
	#' This function returns a dataframe or json object of a GuidanceOfficeVisitComment
	#' @param GuidanceOfficeVisitCommentID The ID of the GuidanceOfficeVisitComment to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given GuidanceOfficeVisitComment. Defaults to FALSE for all return fields which, for convenience, returns all fields for the GuidanceOfficeVisitComment.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('GuidanceOfficeVisitComment') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Guidance
	#' @return A dataframe or of GuidanceOfficeVisitComment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getGuidanceOfficeVisitComment <- function(GuidanceOfficeVisitCommentID, Code = F, CodeDescription = F, CreatedTime = F, Description = F, DistrictGroupKey = F, DistrictID = F, IsActive = F, ModifiedTime = F, OfficeVisitCommentID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "GuidanceOfficeVisitCommentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Guidance", objectName = "OfficeVisitComment", objectId = GuidanceOfficeVisitCommentID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a GuidanceOfficeVisitComment
	#'
	#' This function deletes a GuidanceOfficeVisitComment
	#' @param GuidanceOfficeVisitCommentID The ID of the GuidanceOfficeVisitComment to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Guidance
	#' @return The GuidanceOfficeVisitCommentID of the deleted GuidanceOfficeVisitComment.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteGuidanceOfficeVisitComment <- function(GuidanceOfficeVisitCommentID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Guidance", objectName = "OfficeVisitComment", objectId = GuidanceOfficeVisitCommentID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a GuidanceOfficeVisitComment
	#'
	#' This function creates a GuidanceOfficeVisitComment
	#' @param fieldNames The field values to give the created GuidanceOfficeVisitComment. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Guidance
	#' @return A newly created GuidanceOfficeVisitComment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createGuidanceOfficeVisitComment <- function(Code = NULL, Description = NULL, DistrictGroupKey = NULL, DistrictID = NULL, IsActive = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Guidance", objectName = "OfficeVisitComment", body = list(DataObject = body), searchFields = append("OfficeVisitCommentID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a GuidanceOfficeVisitComment
	#'
	#' This function modifies a GuidanceOfficeVisitComment
	#' @param fieldNames The field values to give the modified GuidanceOfficeVisitComment. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Guidance
	#' @return The modified GuidanceOfficeVisitComment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyGuidanceOfficeVisitComment <- function(OfficeVisitCommentID, Code = NULL, Description = NULL, DistrictGroupKey = NULL, DistrictID = NULL, IsActive = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Guidance", objectName = "OfficeVisitComment", objectId = OfficeVisitCommentID, body = list(DataObject = body), searchFields = append("OfficeVisitCommentID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List GuidanceOfficeVisitReasons
	#'
	#' This function returns a dataframe or json object of GuidanceOfficeVisitReasons
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given GuidanceOfficeVisitReasons. Defaults to FALSE for all return fields which, for convenience, returns all fields for the GuidanceOfficeVisitReasons.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('GuidanceOfficeVisitReason') to get more field paths.
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
	#' @concept Guidance
	#' @return A list of GuidanceOfficeVisitReasons
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listGuidanceOfficeVisitReasons <- function(searchConditionsList = NULL, Code = F, CodeDescription = F, CreatedTime = F, Description = F, DistrictGroupKey = F, DistrictID = F, IsActive = F, ModifiedTime = F, OfficeVisitReasonID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Guidance", objectName = "OfficeVisitReason", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a GuidanceOfficeVisitReason
	#'
	#' This function returns a dataframe or json object of a GuidanceOfficeVisitReason
	#' @param GuidanceOfficeVisitReasonID The ID of the GuidanceOfficeVisitReason to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given GuidanceOfficeVisitReason. Defaults to FALSE for all return fields which, for convenience, returns all fields for the GuidanceOfficeVisitReason.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('GuidanceOfficeVisitReason') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Guidance
	#' @return A dataframe or of GuidanceOfficeVisitReason
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getGuidanceOfficeVisitReason <- function(GuidanceOfficeVisitReasonID, Code = F, CodeDescription = F, CreatedTime = F, Description = F, DistrictGroupKey = F, DistrictID = F, IsActive = F, ModifiedTime = F, OfficeVisitReasonID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "GuidanceOfficeVisitReasonID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Guidance", objectName = "OfficeVisitReason", objectId = GuidanceOfficeVisitReasonID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a GuidanceOfficeVisitReason
	#'
	#' This function deletes a GuidanceOfficeVisitReason
	#' @param GuidanceOfficeVisitReasonID The ID of the GuidanceOfficeVisitReason to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Guidance
	#' @return The GuidanceOfficeVisitReasonID of the deleted GuidanceOfficeVisitReason.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteGuidanceOfficeVisitReason <- function(GuidanceOfficeVisitReasonID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Guidance", objectName = "OfficeVisitReason", objectId = GuidanceOfficeVisitReasonID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a GuidanceOfficeVisitReason
	#'
	#' This function creates a GuidanceOfficeVisitReason
	#' @param fieldNames The field values to give the created GuidanceOfficeVisitReason. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Guidance
	#' @return A newly created GuidanceOfficeVisitReason
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createGuidanceOfficeVisitReason <- function(Code = NULL, Description = NULL, DistrictGroupKey = NULL, DistrictID = NULL, IsActive = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Guidance", objectName = "OfficeVisitReason", body = list(DataObject = body), searchFields = append("OfficeVisitReasonID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a GuidanceOfficeVisitReason
	#'
	#' This function modifies a GuidanceOfficeVisitReason
	#' @param fieldNames The field values to give the modified GuidanceOfficeVisitReason. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Guidance
	#' @return The modified GuidanceOfficeVisitReason
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyGuidanceOfficeVisitReason <- function(OfficeVisitReasonID, Code = NULL, Description = NULL, DistrictGroupKey = NULL, DistrictID = NULL, IsActive = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Guidance", objectName = "OfficeVisitReason", objectId = OfficeVisitReasonID, body = list(DataObject = body), searchFields = append("OfficeVisitReasonID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List GuidanceNotificationMethods
	#'
	#' This function returns a dataframe or json object of GuidanceNotificationMethods
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given GuidanceNotificationMethods. Defaults to FALSE for all return fields which, for convenience, returns all fields for the GuidanceNotificationMethods.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('GuidanceNotificationMethod') to get more field paths.
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
	#' @concept Guidance
	#' @return A list of GuidanceNotificationMethods
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listGuidanceNotificationMethods <- function(searchConditionsList = NULL, Code = F, CodeDescription = F, CreatedTime = F, Description = F, DistrictGroupKey = F, DistrictID = F, IsActive = F, ModifiedTime = F, NotificationMethodID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Guidance", objectName = "NotificationMethod", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a GuidanceNotificationMethod
	#'
	#' This function returns a dataframe or json object of a GuidanceNotificationMethod
	#' @param GuidanceNotificationMethodID The ID of the GuidanceNotificationMethod to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given GuidanceNotificationMethod. Defaults to FALSE for all return fields which, for convenience, returns all fields for the GuidanceNotificationMethod.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('GuidanceNotificationMethod') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Guidance
	#' @return A dataframe or of GuidanceNotificationMethod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getGuidanceNotificationMethod <- function(GuidanceNotificationMethodID, Code = F, CodeDescription = F, CreatedTime = F, Description = F, DistrictGroupKey = F, DistrictID = F, IsActive = F, ModifiedTime = F, NotificationMethodID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "GuidanceNotificationMethodID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Guidance", objectName = "NotificationMethod", objectId = GuidanceNotificationMethodID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a GuidanceNotificationMethod
	#'
	#' This function deletes a GuidanceNotificationMethod
	#' @param GuidanceNotificationMethodID The ID of the GuidanceNotificationMethod to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Guidance
	#' @return The GuidanceNotificationMethodID of the deleted GuidanceNotificationMethod.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteGuidanceNotificationMethod <- function(GuidanceNotificationMethodID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Guidance", objectName = "NotificationMethod", objectId = GuidanceNotificationMethodID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a GuidanceNotificationMethod
	#'
	#' This function creates a GuidanceNotificationMethod
	#' @param fieldNames The field values to give the created GuidanceNotificationMethod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Guidance
	#' @return A newly created GuidanceNotificationMethod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createGuidanceNotificationMethod <- function(Code = NULL, Description = NULL, DistrictGroupKey = NULL, DistrictID = NULL, IsActive = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Guidance", objectName = "NotificationMethod", body = list(DataObject = body), searchFields = append("NotificationMethodID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a GuidanceNotificationMethod
	#'
	#' This function modifies a GuidanceNotificationMethod
	#' @param fieldNames The field values to give the modified GuidanceNotificationMethod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Guidance
	#' @return The modified GuidanceNotificationMethod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyGuidanceNotificationMethod <- function(NotificationMethodID, Code = NULL, Description = NULL, DistrictGroupKey = NULL, DistrictID = NULL, IsActive = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Guidance", objectName = "NotificationMethod", objectId = NotificationMethodID, body = list(DataObject = body), searchFields = append("NotificationMethodID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List GuidanceOfficeVisitGuardianResponses
	#'
	#' This function returns a dataframe or json object of GuidanceOfficeVisitGuardianResponses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given GuidanceOfficeVisitGuardianResponses. Defaults to FALSE for all return fields which, for convenience, returns all fields for the GuidanceOfficeVisitGuardianResponses.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('GuidanceOfficeVisitGuardianResponse') to get more field paths.
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
	#' @concept Guidance
	#' @return A list of GuidanceOfficeVisitGuardianResponses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listGuidanceOfficeVisitGuardianResponses <- function(searchConditionsList = NULL, Code = F, CodeDescription = F, CreatedTime = F, Description = F, DistrictGroupKey = F, DistrictID = F, IsActive = F, ModifiedTime = F, OfficeVisitGuardianResponseID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Guidance", objectName = "OfficeVisitGuardianResponse", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a GuidanceOfficeVisitGuardianResponse
	#'
	#' This function returns a dataframe or json object of a GuidanceOfficeVisitGuardianResponse
	#' @param GuidanceOfficeVisitGuardianResponseID The ID of the GuidanceOfficeVisitGuardianResponse to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given GuidanceOfficeVisitGuardianResponse. Defaults to FALSE for all return fields which, for convenience, returns all fields for the GuidanceOfficeVisitGuardianResponse.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('GuidanceOfficeVisitGuardianResponse') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Guidance
	#' @return A dataframe or of GuidanceOfficeVisitGuardianResponse
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getGuidanceOfficeVisitGuardianResponse <- function(GuidanceOfficeVisitGuardianResponseID, Code = F, CodeDescription = F, CreatedTime = F, Description = F, DistrictGroupKey = F, DistrictID = F, IsActive = F, ModifiedTime = F, OfficeVisitGuardianResponseID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "GuidanceOfficeVisitGuardianResponseID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Guidance", objectName = "OfficeVisitGuardianResponse", objectId = GuidanceOfficeVisitGuardianResponseID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a GuidanceOfficeVisitGuardianResponse
	#'
	#' This function deletes a GuidanceOfficeVisitGuardianResponse
	#' @param GuidanceOfficeVisitGuardianResponseID The ID of the GuidanceOfficeVisitGuardianResponse to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Guidance
	#' @return The GuidanceOfficeVisitGuardianResponseID of the deleted GuidanceOfficeVisitGuardianResponse.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteGuidanceOfficeVisitGuardianResponse <- function(GuidanceOfficeVisitGuardianResponseID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Guidance", objectName = "OfficeVisitGuardianResponse", objectId = GuidanceOfficeVisitGuardianResponseID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a GuidanceOfficeVisitGuardianResponse
	#'
	#' This function creates a GuidanceOfficeVisitGuardianResponse
	#' @param fieldNames The field values to give the created GuidanceOfficeVisitGuardianResponse. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Guidance
	#' @return A newly created GuidanceOfficeVisitGuardianResponse
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createGuidanceOfficeVisitGuardianResponse <- function(Code = NULL, Description = NULL, DistrictGroupKey = NULL, DistrictID = NULL, IsActive = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Guidance", objectName = "OfficeVisitGuardianResponse", body = list(DataObject = body), searchFields = append("OfficeVisitGuardianResponseID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a GuidanceOfficeVisitGuardianResponse
	#'
	#' This function modifies a GuidanceOfficeVisitGuardianResponse
	#' @param fieldNames The field values to give the modified GuidanceOfficeVisitGuardianResponse. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Guidance
	#' @return The modified GuidanceOfficeVisitGuardianResponse
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyGuidanceOfficeVisitGuardianResponse <- function(OfficeVisitGuardianResponseID, Code = NULL, Description = NULL, DistrictGroupKey = NULL, DistrictID = NULL, IsActive = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Guidance", objectName = "OfficeVisitGuardianResponse", objectId = OfficeVisitGuardianResponseID, body = list(DataObject = body), searchFields = append("OfficeVisitGuardianResponseID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}
