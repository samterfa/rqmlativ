
	#' List QueuedCompletedGradeChangeNotifications
	#'
	#' This function returns a dataframe or json object of QueuedCompletedGradeChangeNotifications
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given QueuedCompletedGradeChangeNotifications. Defaults to FALSE for all return fields which, for convenience, returns all fields for the QueuedCompletedGradeChangeNotifications.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('QueuedCompletedGradeChangeNotification') to get more field paths.
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
	#' @concept MessageCenter
	#' @return A list of QueuedCompletedGradeChangeNotifications
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listQueuedCompletedGradeChangeNotifications <- function(searchConditionsList = NULL, CreatedTime = F, EntityID = F, GradeMarkIDCurrent = F, GradeMarkIDPrevious = F, IsSent = F, ModifiedTime = F, NotificationID = F, QueuedCompletedGradeChangeNotificationID = F, SchoolYearID = F, StudentGradeBucketID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "QueuedCompletedGradeChangeNotification", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a QueuedCompletedGradeChangeNotification
	#'
	#' This function returns a dataframe or json object of a QueuedCompletedGradeChangeNotification
	#' @param QueuedCompletedGradeChangeNotificationID The ID of the QueuedCompletedGradeChangeNotification to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given QueuedCompletedGradeChangeNotification. Defaults to FALSE for all return fields which, for convenience, returns all fields for the QueuedCompletedGradeChangeNotification.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('QueuedCompletedGradeChangeNotification') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A dataframe or of QueuedCompletedGradeChangeNotification
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getQueuedCompletedGradeChangeNotification <- function(QueuedCompletedGradeChangeNotificationID, CreatedTime = F, EntityID = F, GradeMarkIDCurrent = F, GradeMarkIDPrevious = F, IsSent = F, ModifiedTime = F, NotificationID = F, SchoolYearID = F, StudentGradeBucketID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "QueuedCompletedGradeChangeNotificationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "QueuedCompletedGradeChangeNotification", objectId = QueuedCompletedGradeChangeNotificationID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a QueuedCompletedGradeChangeNotification
	#'
	#' This function deletes a QueuedCompletedGradeChangeNotification
	#' @param QueuedCompletedGradeChangeNotificationID The ID of the QueuedCompletedGradeChangeNotification to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The QueuedCompletedGradeChangeNotificationID of the deleted QueuedCompletedGradeChangeNotification.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteQueuedCompletedGradeChangeNotification <- function(QueuedCompletedGradeChangeNotificationID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "QueuedCompletedGradeChangeNotification", objectId = QueuedCompletedGradeChangeNotificationID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a QueuedCompletedGradeChangeNotification
	#'
	#' This function creates a QueuedCompletedGradeChangeNotification
	#' @param fieldNames The field values to give the created QueuedCompletedGradeChangeNotification. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A newly created QueuedCompletedGradeChangeNotification
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createQueuedCompletedGradeChangeNotification <- function(EntityID = NULL, GradeMarkIDCurrent = NULL, GradeMarkIDPrevious = NULL, IsSent = NULL, NotificationID = NULL, SchoolYearID = NULL, StudentGradeBucketID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "QueuedCompletedGradeChangeNotification", body = list(DataObject = body), searchFields = append("QueuedCompletedGradeChangeNotificationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a QueuedCompletedGradeChangeNotification
	#'
	#' This function modifies a QueuedCompletedGradeChangeNotification
	#' @param fieldNames The field values to give the modified QueuedCompletedGradeChangeNotification. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The modified QueuedCompletedGradeChangeNotification
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyQueuedCompletedGradeChangeNotification <- function(QueuedCompletedGradeChangeNotificationID, EntityID = NULL, GradeMarkIDCurrent = NULL, GradeMarkIDPrevious = NULL, IsSent = NULL, NotificationID = NULL, SchoolYearID = NULL, StudentGradeBucketID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "QueuedCompletedGradeChangeNotification", objectId = QueuedCompletedGradeChangeNotificationID, body = list(DataObject = body), searchFields = append("QueuedCompletedGradeChangeNotificationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List NotificationDisciplineThresholds
	#'
	#' This function returns a dataframe or json object of NotificationDisciplineThresholds
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NotificationDisciplineThresholds. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NotificationDisciplineThresholds.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NotificationDisciplineThreshold') to get more field paths.
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
	#' @concept MessageCenter
	#' @return A list of NotificationDisciplineThresholds
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNotificationDisciplineThresholds <- function(searchConditionsList = NULL, CreatedTime = F, DisciplineThresholdID = F, ModifiedTime = F, NotificationDisciplineThresholdID = F, NotificationID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "NotificationDisciplineThreshold", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NotificationDisciplineThreshold
	#'
	#' This function returns a dataframe or json object of a NotificationDisciplineThreshold
	#' @param NotificationDisciplineThresholdID The ID of the NotificationDisciplineThreshold to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NotificationDisciplineThreshold. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NotificationDisciplineThreshold.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NotificationDisciplineThreshold') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A dataframe or of NotificationDisciplineThreshold
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNotificationDisciplineThreshold <- function(NotificationDisciplineThresholdID, CreatedTime = F, DisciplineThresholdID = F, ModifiedTime = F, NotificationID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NotificationDisciplineThresholdID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "NotificationDisciplineThreshold", objectId = NotificationDisciplineThresholdID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NotificationDisciplineThreshold
	#'
	#' This function deletes a NotificationDisciplineThreshold
	#' @param NotificationDisciplineThresholdID The ID of the NotificationDisciplineThreshold to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The NotificationDisciplineThresholdID of the deleted NotificationDisciplineThreshold.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNotificationDisciplineThreshold <- function(NotificationDisciplineThresholdID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "NotificationDisciplineThreshold", objectId = NotificationDisciplineThresholdID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NotificationDisciplineThreshold
	#'
	#' This function creates a NotificationDisciplineThreshold
	#' @param fieldNames The field values to give the created NotificationDisciplineThreshold. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A newly created NotificationDisciplineThreshold
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNotificationDisciplineThreshold <- function(DisciplineThresholdID = NULL, NotificationID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "NotificationDisciplineThreshold", body = list(DataObject = body), searchFields = append("NotificationDisciplineThresholdID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NotificationDisciplineThreshold
	#'
	#' This function modifies a NotificationDisciplineThreshold
	#' @param fieldNames The field values to give the modified NotificationDisciplineThreshold. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The modified NotificationDisciplineThreshold
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNotificationDisciplineThreshold <- function(NotificationDisciplineThresholdID, DisciplineThresholdID = NULL, NotificationID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "NotificationDisciplineThreshold", objectId = NotificationDisciplineThresholdID, body = list(DataObject = body), searchFields = append("NotificationDisciplineThresholdID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List NotificationGradeReferences
	#'
	#' This function returns a dataframe or json object of NotificationGradeReferences
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NotificationGradeReferences. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NotificationGradeReferences.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NotificationGradeReference') to get more field paths.
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
	#' @concept MessageCenter
	#' @return A list of NotificationGradeReferences
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNotificationGradeReferences <- function(searchConditionsList = NULL, CreatedTime = F, GradeReferenceID = F, ModifiedTime = F, NotificationGradeReferenceID = F, NotificationID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "NotificationGradeReference", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NotificationGradeReference
	#'
	#' This function returns a dataframe or json object of a NotificationGradeReference
	#' @param NotificationGradeReferenceID The ID of the NotificationGradeReference to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NotificationGradeReference. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NotificationGradeReference.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NotificationGradeReference') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A dataframe or of NotificationGradeReference
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNotificationGradeReference <- function(NotificationGradeReferenceID, CreatedTime = F, GradeReferenceID = F, ModifiedTime = F, NotificationID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NotificationGradeReferenceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "NotificationGradeReference", objectId = NotificationGradeReferenceID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NotificationGradeReference
	#'
	#' This function deletes a NotificationGradeReference
	#' @param NotificationGradeReferenceID The ID of the NotificationGradeReference to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The NotificationGradeReferenceID of the deleted NotificationGradeReference.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNotificationGradeReference <- function(NotificationGradeReferenceID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "NotificationGradeReference", objectId = NotificationGradeReferenceID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NotificationGradeReference
	#'
	#' This function creates a NotificationGradeReference
	#' @param fieldNames The field values to give the created NotificationGradeReference. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A newly created NotificationGradeReference
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNotificationGradeReference <- function(GradeReferenceID = NULL, NotificationID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "NotificationGradeReference", body = list(DataObject = body), searchFields = append("NotificationGradeReferenceID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NotificationGradeReference
	#'
	#' This function modifies a NotificationGradeReference
	#' @param fieldNames The field values to give the modified NotificationGradeReference. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The modified NotificationGradeReference
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNotificationGradeReference <- function(NotificationGradeReferenceID, GradeReferenceID = NULL, NotificationID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "NotificationGradeReference", objectId = NotificationGradeReferenceID, body = list(DataObject = body), searchFields = append("NotificationGradeReferenceID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List NotificationUnrecordedClassAttendances
	#'
	#' This function returns a dataframe or json object of NotificationUnrecordedClassAttendances
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NotificationUnrecordedClassAttendances. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NotificationUnrecordedClassAttendances.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NotificationUnrecordedClassAttendance') to get more field paths.
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
	#' @concept MessageCenter
	#' @return A list of NotificationUnrecordedClassAttendances
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNotificationUnrecordedClassAttendances <- function(searchConditionsList = NULL, CreatedTime = F, DisplayPeriodID = F, ModifiedTime = F, NotificationID = F, NotificationUnrecordedClassAttendanceID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "NotificationUnrecordedClassAttendance", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NotificationUnrecordedClassAttendance
	#'
	#' This function returns a dataframe or json object of a NotificationUnrecordedClassAttendance
	#' @param NotificationUnrecordedClassAttendanceID The ID of the NotificationUnrecordedClassAttendance to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NotificationUnrecordedClassAttendance. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NotificationUnrecordedClassAttendance.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NotificationUnrecordedClassAttendance') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A dataframe or of NotificationUnrecordedClassAttendance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNotificationUnrecordedClassAttendance <- function(NotificationUnrecordedClassAttendanceID, CreatedTime = F, DisplayPeriodID = F, ModifiedTime = F, NotificationID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NotificationUnrecordedClassAttendanceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "NotificationUnrecordedClassAttendance", objectId = NotificationUnrecordedClassAttendanceID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NotificationUnrecordedClassAttendance
	#'
	#' This function deletes a NotificationUnrecordedClassAttendance
	#' @param NotificationUnrecordedClassAttendanceID The ID of the NotificationUnrecordedClassAttendance to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The NotificationUnrecordedClassAttendanceID of the deleted NotificationUnrecordedClassAttendance.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNotificationUnrecordedClassAttendance <- function(NotificationUnrecordedClassAttendanceID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "NotificationUnrecordedClassAttendance", objectId = NotificationUnrecordedClassAttendanceID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NotificationUnrecordedClassAttendance
	#'
	#' This function creates a NotificationUnrecordedClassAttendance
	#' @param fieldNames The field values to give the created NotificationUnrecordedClassAttendance. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A newly created NotificationUnrecordedClassAttendance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNotificationUnrecordedClassAttendance <- function(DisplayPeriodID = NULL, NotificationID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "NotificationUnrecordedClassAttendance", body = list(DataObject = body), searchFields = append("NotificationUnrecordedClassAttendanceID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NotificationUnrecordedClassAttendance
	#'
	#' This function modifies a NotificationUnrecordedClassAttendance
	#' @param fieldNames The field values to give the modified NotificationUnrecordedClassAttendance. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The modified NotificationUnrecordedClassAttendance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNotificationUnrecordedClassAttendance <- function(NotificationUnrecordedClassAttendanceID, DisplayPeriodID = NULL, NotificationID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "NotificationUnrecordedClassAttendance", objectId = NotificationUnrecordedClassAttendanceID, body = list(DataObject = body), searchFields = append("NotificationUnrecordedClassAttendanceID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List NotificationScheduleIsAvailableSectionLengths
	#'
	#' This function returns a dataframe or json object of NotificationScheduleIsAvailableSectionLengths
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NotificationScheduleIsAvailableSectionLengths. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NotificationScheduleIsAvailableSectionLengths.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NotificationScheduleIsAvailableSectionLength') to get more field paths.
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
	#' @concept MessageCenter
	#' @return A list of NotificationScheduleIsAvailableSectionLengths
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNotificationScheduleIsAvailableSectionLengths <- function(searchConditionsList = NULL, CreatedTime = F, ModifiedTime = F, NotificationID = F, NotificationScheduleIsAvailableSectionLengthID = F, SectionLengthID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "NotificationScheduleIsAvailableSectionLength", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NotificationScheduleIsAvailableSectionLength
	#'
	#' This function returns a dataframe or json object of a NotificationScheduleIsAvailableSectionLength
	#' @param NotificationScheduleIsAvailableSectionLengthID The ID of the NotificationScheduleIsAvailableSectionLength to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NotificationScheduleIsAvailableSectionLength. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NotificationScheduleIsAvailableSectionLength.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NotificationScheduleIsAvailableSectionLength') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A dataframe or of NotificationScheduleIsAvailableSectionLength
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNotificationScheduleIsAvailableSectionLength <- function(NotificationScheduleIsAvailableSectionLengthID, CreatedTime = F, ModifiedTime = F, NotificationID = F, SectionLengthID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NotificationScheduleIsAvailableSectionLengthID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "NotificationScheduleIsAvailableSectionLength", objectId = NotificationScheduleIsAvailableSectionLengthID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NotificationScheduleIsAvailableSectionLength
	#'
	#' This function deletes a NotificationScheduleIsAvailableSectionLength
	#' @param NotificationScheduleIsAvailableSectionLengthID The ID of the NotificationScheduleIsAvailableSectionLength to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The NotificationScheduleIsAvailableSectionLengthID of the deleted NotificationScheduleIsAvailableSectionLength.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNotificationScheduleIsAvailableSectionLength <- function(NotificationScheduleIsAvailableSectionLengthID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "NotificationScheduleIsAvailableSectionLength", objectId = NotificationScheduleIsAvailableSectionLengthID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NotificationScheduleIsAvailableSectionLength
	#'
	#' This function creates a NotificationScheduleIsAvailableSectionLength
	#' @param fieldNames The field values to give the created NotificationScheduleIsAvailableSectionLength. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A newly created NotificationScheduleIsAvailableSectionLength
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNotificationScheduleIsAvailableSectionLength <- function(NotificationID = NULL, SectionLengthID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "NotificationScheduleIsAvailableSectionLength", body = list(DataObject = body), searchFields = append("NotificationScheduleIsAvailableSectionLengthID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NotificationScheduleIsAvailableSectionLength
	#'
	#' This function modifies a NotificationScheduleIsAvailableSectionLength
	#' @param fieldNames The field values to give the modified NotificationScheduleIsAvailableSectionLength. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The modified NotificationScheduleIsAvailableSectionLength
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNotificationScheduleIsAvailableSectionLength <- function(NotificationScheduleIsAvailableSectionLengthID, NotificationID = NULL, SectionLengthID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "NotificationScheduleIsAvailableSectionLength", objectId = NotificationScheduleIsAvailableSectionLengthID, body = list(DataObject = body), searchFields = append("NotificationScheduleIsAvailableSectionLengthID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List NotificationOnlineForms
	#'
	#' This function returns a dataframe or json object of NotificationOnlineForms
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NotificationOnlineForms. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NotificationOnlineForms.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NotificationOnlineForm') to get more field paths.
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
	#' @concept MessageCenter
	#' @return A list of NotificationOnlineForms
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNotificationOnlineForms <- function(searchConditionsList = NULL, CreatedTime = F, ModifiedTime = F, NotificationID = F, NotificationOnlineFormID = F, OnlineFormID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "NotificationOnlineForm", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NotificationOnlineForm
	#'
	#' This function returns a dataframe or json object of a NotificationOnlineForm
	#' @param NotificationOnlineFormID The ID of the NotificationOnlineForm to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NotificationOnlineForm. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NotificationOnlineForm.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NotificationOnlineForm') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A dataframe or of NotificationOnlineForm
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNotificationOnlineForm <- function(NotificationOnlineFormID, CreatedTime = F, ModifiedTime = F, NotificationID = F, OnlineFormID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NotificationOnlineFormID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "NotificationOnlineForm", objectId = NotificationOnlineFormID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NotificationOnlineForm
	#'
	#' This function deletes a NotificationOnlineForm
	#' @param NotificationOnlineFormID The ID of the NotificationOnlineForm to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The NotificationOnlineFormID of the deleted NotificationOnlineForm.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNotificationOnlineForm <- function(NotificationOnlineFormID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "NotificationOnlineForm", objectId = NotificationOnlineFormID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NotificationOnlineForm
	#'
	#' This function creates a NotificationOnlineForm
	#' @param fieldNames The field values to give the created NotificationOnlineForm. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A newly created NotificationOnlineForm
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNotificationOnlineForm <- function(NotificationID = NULL, OnlineFormID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "NotificationOnlineForm", body = list(DataObject = body), searchFields = append("NotificationOnlineFormID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NotificationOnlineForm
	#'
	#' This function modifies a NotificationOnlineForm
	#' @param fieldNames The field values to give the modified NotificationOnlineForm. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The modified NotificationOnlineForm
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNotificationOnlineForm <- function(NotificationOnlineFormID, NotificationID = NULL, OnlineFormID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "NotificationOnlineForm", objectId = NotificationOnlineFormID, body = list(DataObject = body), searchFields = append("NotificationOnlineFormID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List NotificationCarbonCopyStaffs
	#'
	#' This function returns a dataframe or json object of NotificationCarbonCopyStaffs
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NotificationCarbonCopyStaffs. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NotificationCarbonCopyStaffs.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NotificationCarbonCopyStaff') to get more field paths.
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
	#' @concept MessageCenter
	#' @return A list of NotificationCarbonCopyStaffs
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNotificationCarbonCopyStaffs <- function(searchConditionsList = NULL, CreatedTime = F, ModifiedTime = F, NotificationCarbonCopyStaffID = F, NotificationID = F, StaffID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "NotificationCarbonCopyStaff", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NotificationCarbonCopyStaff
	#'
	#' This function returns a dataframe or json object of a NotificationCarbonCopyStaff
	#' @param NotificationCarbonCopyStaffID The ID of the NotificationCarbonCopyStaff to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NotificationCarbonCopyStaff. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NotificationCarbonCopyStaff.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NotificationCarbonCopyStaff') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A dataframe or of NotificationCarbonCopyStaff
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNotificationCarbonCopyStaff <- function(NotificationCarbonCopyStaffID, CreatedTime = F, ModifiedTime = F, NotificationID = F, StaffID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NotificationCarbonCopyStaffID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "NotificationCarbonCopyStaff", objectId = NotificationCarbonCopyStaffID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NotificationCarbonCopyStaff
	#'
	#' This function deletes a NotificationCarbonCopyStaff
	#' @param NotificationCarbonCopyStaffID The ID of the NotificationCarbonCopyStaff to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The NotificationCarbonCopyStaffID of the deleted NotificationCarbonCopyStaff.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNotificationCarbonCopyStaff <- function(NotificationCarbonCopyStaffID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "NotificationCarbonCopyStaff", objectId = NotificationCarbonCopyStaffID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NotificationCarbonCopyStaff
	#'
	#' This function creates a NotificationCarbonCopyStaff
	#' @param fieldNames The field values to give the created NotificationCarbonCopyStaff. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A newly created NotificationCarbonCopyStaff
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNotificationCarbonCopyStaff <- function(NotificationID = NULL, StaffID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "NotificationCarbonCopyStaff", body = list(DataObject = body), searchFields = append("NotificationCarbonCopyStaffID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NotificationCarbonCopyStaff
	#'
	#' This function modifies a NotificationCarbonCopyStaff
	#' @param fieldNames The field values to give the modified NotificationCarbonCopyStaff. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The modified NotificationCarbonCopyStaff
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNotificationCarbonCopyStaff <- function(NotificationCarbonCopyStaffID, NotificationID = NULL, StaffID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "NotificationCarbonCopyStaff", objectId = NotificationCarbonCopyStaffID, body = list(DataObject = body), searchFields = append("NotificationCarbonCopyStaffID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List NotificationActions
	#'
	#' This function returns a dataframe or json object of NotificationActions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NotificationActions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NotificationActions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NotificationAction') to get more field paths.
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
	#' @concept MessageCenter
	#' @return A list of NotificationActions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNotificationActions <- function(searchConditionsList = NULL, ActionID = F, CreatedTime = F, ModifiedTime = F, NotificationActionID = F, NotificationID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "NotificationAction", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NotificationAction
	#'
	#' This function returns a dataframe or json object of a NotificationAction
	#' @param NotificationActionID The ID of the NotificationAction to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NotificationAction. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NotificationAction.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NotificationAction') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A dataframe or of NotificationAction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNotificationAction <- function(NotificationActionID, ActionID = F, CreatedTime = F, ModifiedTime = F, NotificationID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NotificationActionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "NotificationAction", objectId = NotificationActionID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NotificationAction
	#'
	#' This function deletes a NotificationAction
	#' @param NotificationActionID The ID of the NotificationAction to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The NotificationActionID of the deleted NotificationAction.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNotificationAction <- function(NotificationActionID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "NotificationAction", objectId = NotificationActionID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NotificationAction
	#'
	#' This function creates a NotificationAction
	#' @param fieldNames The field values to give the created NotificationAction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A newly created NotificationAction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNotificationAction <- function(ActionID = NULL, NotificationID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "NotificationAction", body = list(DataObject = body), searchFields = append("NotificationActionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NotificationAction
	#'
	#' This function modifies a NotificationAction
	#' @param fieldNames The field values to give the modified NotificationAction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The modified NotificationAction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNotificationAction <- function(NotificationActionID, ActionID = NULL, NotificationID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "NotificationAction", objectId = NotificationActionID, body = list(DataObject = body), searchFields = append("NotificationActionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List NotificationAttendanceTypes
	#'
	#' This function returns a dataframe or json object of NotificationAttendanceTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NotificationAttendanceTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NotificationAttendanceTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NotificationAttendanceType') to get more field paths.
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
	#' @concept MessageCenter
	#' @return A list of NotificationAttendanceTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNotificationAttendanceTypes <- function(searchConditionsList = NULL, AttendanceTypeID = F, CreatedTime = F, ModifiedTime = F, NotificationAttendanceTypeID = F, NotificationID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "NotificationAttendanceType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NotificationAttendanceType
	#'
	#' This function returns a dataframe or json object of a NotificationAttendanceType
	#' @param NotificationAttendanceTypeID The ID of the NotificationAttendanceType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NotificationAttendanceType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NotificationAttendanceType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NotificationAttendanceType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A dataframe or of NotificationAttendanceType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNotificationAttendanceType <- function(NotificationAttendanceTypeID, AttendanceTypeID = F, CreatedTime = F, ModifiedTime = F, NotificationID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NotificationAttendanceTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "NotificationAttendanceType", objectId = NotificationAttendanceTypeID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NotificationAttendanceType
	#'
	#' This function deletes a NotificationAttendanceType
	#' @param NotificationAttendanceTypeID The ID of the NotificationAttendanceType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The NotificationAttendanceTypeID of the deleted NotificationAttendanceType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNotificationAttendanceType <- function(NotificationAttendanceTypeID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "NotificationAttendanceType", objectId = NotificationAttendanceTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NotificationAttendanceType
	#'
	#' This function creates a NotificationAttendanceType
	#' @param fieldNames The field values to give the created NotificationAttendanceType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A newly created NotificationAttendanceType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNotificationAttendanceType <- function(AttendanceTypeID = NULL, NotificationID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "NotificationAttendanceType", body = list(DataObject = body), searchFields = append("NotificationAttendanceTypeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NotificationAttendanceType
	#'
	#' This function modifies a NotificationAttendanceType
	#' @param fieldNames The field values to give the modified NotificationAttendanceType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The modified NotificationAttendanceType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNotificationAttendanceType <- function(NotificationAttendanceTypeID, AttendanceTypeID = NULL, NotificationID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "NotificationAttendanceType", objectId = NotificationAttendanceTypeID, body = list(DataObject = body), searchFields = append("NotificationAttendanceTypeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List NotificationGradeBuckets
	#'
	#' This function returns a dataframe or json object of NotificationGradeBuckets
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NotificationGradeBuckets. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NotificationGradeBuckets.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NotificationGradeBucket') to get more field paths.
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
	#' @concept MessageCenter
	#' @return A list of NotificationGradeBuckets
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNotificationGradeBuckets <- function(searchConditionsList = NULL, CreatedTime = F, GradeBucketID = F, ModifiedTime = F, NotificationGradeBucketID = F, NotificationID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "NotificationGradeBucket", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NotificationGradeBucket
	#'
	#' This function returns a dataframe or json object of a NotificationGradeBucket
	#' @param NotificationGradeBucketID The ID of the NotificationGradeBucket to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NotificationGradeBucket. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NotificationGradeBucket.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NotificationGradeBucket') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A dataframe or of NotificationGradeBucket
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNotificationGradeBucket <- function(NotificationGradeBucketID, CreatedTime = F, GradeBucketID = F, ModifiedTime = F, NotificationID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NotificationGradeBucketID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "NotificationGradeBucket", objectId = NotificationGradeBucketID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NotificationGradeBucket
	#'
	#' This function deletes a NotificationGradeBucket
	#' @param NotificationGradeBucketID The ID of the NotificationGradeBucket to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The NotificationGradeBucketID of the deleted NotificationGradeBucket.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNotificationGradeBucket <- function(NotificationGradeBucketID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "NotificationGradeBucket", objectId = NotificationGradeBucketID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NotificationGradeBucket
	#'
	#' This function creates a NotificationGradeBucket
	#' @param fieldNames The field values to give the created NotificationGradeBucket. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A newly created NotificationGradeBucket
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNotificationGradeBucket <- function(GradeBucketID = NULL, NotificationID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "NotificationGradeBucket", body = list(DataObject = body), searchFields = append("NotificationGradeBucketID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NotificationGradeBucket
	#'
	#' This function modifies a NotificationGradeBucket
	#' @param fieldNames The field values to give the modified NotificationGradeBucket. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The modified NotificationGradeBucket
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNotificationGradeBucket <- function(NotificationGradeBucketID, GradeBucketID = NULL, NotificationID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "NotificationGradeBucket", objectId = NotificationGradeBucketID, body = list(DataObject = body), searchFields = append("NotificationGradeBucketID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List NotificationGradeMarks
	#'
	#' This function returns a dataframe or json object of NotificationGradeMarks
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NotificationGradeMarks. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NotificationGradeMarks.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NotificationGradeMark') to get more field paths.
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
	#' @concept MessageCenter
	#' @return A list of NotificationGradeMarks
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNotificationGradeMarks <- function(searchConditionsList = NULL, CreatedTime = F, GradeMarkID = F, ModifiedTime = F, NotificationGradeMarkID = F, NotificationID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "NotificationGradeMark", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NotificationGradeMark
	#'
	#' This function returns a dataframe or json object of a NotificationGradeMark
	#' @param NotificationGradeMarkID The ID of the NotificationGradeMark to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NotificationGradeMark. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NotificationGradeMark.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NotificationGradeMark') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A dataframe or of NotificationGradeMark
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNotificationGradeMark <- function(NotificationGradeMarkID, CreatedTime = F, GradeMarkID = F, ModifiedTime = F, NotificationID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NotificationGradeMarkID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "NotificationGradeMark", objectId = NotificationGradeMarkID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NotificationGradeMark
	#'
	#' This function deletes a NotificationGradeMark
	#' @param NotificationGradeMarkID The ID of the NotificationGradeMark to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The NotificationGradeMarkID of the deleted NotificationGradeMark.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNotificationGradeMark <- function(NotificationGradeMarkID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "NotificationGradeMark", objectId = NotificationGradeMarkID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NotificationGradeMark
	#'
	#' This function creates a NotificationGradeMark
	#' @param fieldNames The field values to give the created NotificationGradeMark. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A newly created NotificationGradeMark
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNotificationGradeMark <- function(GradeMarkID = NULL, NotificationID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "NotificationGradeMark", body = list(DataObject = body), searchFields = append("NotificationGradeMarkID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NotificationGradeMark
	#'
	#' This function modifies a NotificationGradeMark
	#' @param fieldNames The field values to give the modified NotificationGradeMark. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The modified NotificationGradeMark
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNotificationGradeMark <- function(NotificationGradeMarkID, GradeMarkID = NULL, NotificationID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "NotificationGradeMark", objectId = NotificationGradeMarkID, body = list(DataObject = body), searchFields = append("NotificationGradeMarkID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List NotificationOffenses
	#'
	#' This function returns a dataframe or json object of NotificationOffenses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NotificationOffenses. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NotificationOffenses.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NotificationOffense') to get more field paths.
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
	#' @concept MessageCenter
	#' @return A list of NotificationOffenses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNotificationOffenses <- function(searchConditionsList = NULL, CreatedTime = F, ModifiedTime = F, NotificationID = F, NotificationOffenseID = F, OffenseID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "NotificationOffense", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NotificationOffense
	#'
	#' This function returns a dataframe or json object of a NotificationOffense
	#' @param NotificationOffenseID The ID of the NotificationOffense to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NotificationOffense. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NotificationOffense.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NotificationOffense') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A dataframe or of NotificationOffense
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNotificationOffense <- function(NotificationOffenseID, CreatedTime = F, ModifiedTime = F, NotificationID = F, OffenseID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NotificationOffenseID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "NotificationOffense", objectId = NotificationOffenseID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NotificationOffense
	#'
	#' This function deletes a NotificationOffense
	#' @param NotificationOffenseID The ID of the NotificationOffense to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The NotificationOffenseID of the deleted NotificationOffense.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNotificationOffense <- function(NotificationOffenseID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "NotificationOffense", objectId = NotificationOffenseID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NotificationOffense
	#'
	#' This function creates a NotificationOffense
	#' @param fieldNames The field values to give the created NotificationOffense. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A newly created NotificationOffense
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNotificationOffense <- function(NotificationID = NULL, OffenseID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "NotificationOffense", body = list(DataObject = body), searchFields = append("NotificationOffenseID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NotificationOffense
	#'
	#' This function modifies a NotificationOffense
	#' @param fieldNames The field values to give the modified NotificationOffense. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The modified NotificationOffense
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNotificationOffense <- function(NotificationOffenseID, NotificationID = NULL, OffenseID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "NotificationOffense", objectId = NotificationOffenseID, body = list(DataObject = body), searchFields = append("NotificationOffenseID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempMessages
	#'
	#' This function returns a dataframe or json object of TempMessages
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempMessages. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempMessages.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempMessage') to get more field paths.
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
	#' @concept MessageCenter
	#' @return A list of TempMessages
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempMessages <- function(searchConditionsList = NULL, CreatedTime = F, ModifiedTime = F, RecipientName = F, Relationship = F, SectionInfo = F, StudentName = F, TempMessageID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "TempMessage", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempMessage
	#'
	#' This function returns a dataframe or json object of a TempMessage
	#' @param TempMessageID The ID of the TempMessage to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempMessage. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempMessage.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempMessage') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A dataframe or of TempMessage
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempMessage <- function(TempMessageID, CreatedTime = F, ModifiedTime = F, RecipientName = F, Relationship = F, SectionInfo = F, StudentName = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempMessageID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "TempMessage", objectId = TempMessageID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempMessage
	#'
	#' This function deletes a TempMessage
	#' @param TempMessageID The ID of the TempMessage to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The TempMessageID of the deleted TempMessage.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempMessage <- function(TempMessageID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "TempMessage", objectId = TempMessageID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempMessage
	#'
	#' This function creates a TempMessage
	#' @param fieldNames The field values to give the created TempMessage. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A newly created TempMessage
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempMessage <- function(RecipientName = NULL, Relationship = NULL, SectionInfo = NULL, StudentName = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "TempMessage", body = list(DataObject = body), searchFields = append("TempMessageID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempMessage
	#'
	#' This function modifies a TempMessage
	#' @param fieldNames The field values to give the modified TempMessage. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The modified TempMessage
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempMessage <- function(TempMessageID, RecipientName = NULL, Relationship = NULL, SectionInfo = NULL, StudentName = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "TempMessage", objectId = TempMessageID, body = list(DataObject = body), searchFields = append("TempMessageID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Notifications
	#'
	#' This function returns a dataframe or json object of Notifications
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Notifications. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Notifications.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Notification') to get more field paths.
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
	#' @concept MessageCenter
	#' @return A list of Notifications
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNotifications <- function(searchConditionsList = NULL, AttachmentCount = F, AttendanceCategoryForCount = F, AttendanceCountHigh = F, AttendanceCountLow = F, AttendanceCountMethod = F, ConsiderAllStaffMeets = F, CreatedTime = F, DayType = F, EntityID = F, FeeManagementBalanceHigh = F, FeeManagementBalanceLow = F, FoodServiceBalanceHigh = F, FoodServiceBalanceLow = F, GradingPeriodEndDaysAfter = F, GradingPeriodEndDaysBefore = F, IncludeAutoGeneratedMessage = F, LastEntryType = F, Message = F, ModifiedTime = F, NotificationID = F, NumberOfDays = F, PriorityType = F, ScheduledTaskID = F, ScheduleIsAvailableDaysBefore = F, SchoolYearID = F, SendNotificationForDay = F, SendNotificationForPriorDayCount = F, SendOnlyIfGuardianNotNotified = F, SendToDisciplineOfficer = F, SubjectCleaned = F, SubjectCode = F, ToWhom = F, Type = F, UnrecordedAttendanceMinutes = F, UnrecordedAttendancePeriodType = F, UserIDCreatedBy = F, UserIDModifiedBy = F, WarningMessage = F, XMLFilter = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "Notification", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Notification
	#'
	#' This function returns a dataframe or json object of a Notification
	#' @param NotificationID The ID of the Notification to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Notification. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Notification.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Notification') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A dataframe or of Notification
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNotification <- function(NotificationID, AttachmentCount = F, AttendanceCategoryForCount = F, AttendanceCountHigh = F, AttendanceCountLow = F, AttendanceCountMethod = F, ConsiderAllStaffMeets = F, CreatedTime = F, DayType = F, EntityID = F, FeeManagementBalanceHigh = F, FeeManagementBalanceLow = F, FoodServiceBalanceHigh = F, FoodServiceBalanceLow = F, GradingPeriodEndDaysAfter = F, GradingPeriodEndDaysBefore = F, IncludeAutoGeneratedMessage = F, LastEntryType = F, Message = F, ModifiedTime = F, NumberOfDays = F, PriorityType = F, ScheduledTaskID = F, ScheduleIsAvailableDaysBefore = F, SchoolYearID = F, SendNotificationForDay = F, SendNotificationForPriorDayCount = F, SendOnlyIfGuardianNotNotified = F, SendToDisciplineOfficer = F, SubjectCleaned = F, SubjectCode = F, ToWhom = F, Type = F, UnrecordedAttendanceMinutes = F, UnrecordedAttendancePeriodType = F, UserIDCreatedBy = F, UserIDModifiedBy = F, WarningMessage = F, XMLFilter = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NotificationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "Notification", objectId = NotificationID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Notification
	#'
	#' This function deletes a Notification
	#' @param NotificationID The ID of the Notification to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The NotificationID of the deleted Notification.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNotification <- function(NotificationID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "Notification", objectId = NotificationID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Notification
	#'
	#' This function creates a Notification
	#' @param fieldNames The field values to give the created Notification. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A newly created Notification
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNotification <- function(AttendanceCategoryForCount = NULL, AttendanceCountHigh = NULL, AttendanceCountLow = NULL, AttendanceCountMethod = NULL, ConsiderAllStaffMeets = NULL, DayType = NULL, EntityID = NULL, FeeManagementBalanceHigh = NULL, FeeManagementBalanceLow = NULL, FoodServiceBalanceHigh = NULL, FoodServiceBalanceLow = NULL, GradingPeriodEndDaysAfter = NULL, GradingPeriodEndDaysBefore = NULL, IncludeAutoGeneratedMessage = NULL, LastEntryType = NULL, Message = NULL, NumberOfDays = NULL, PriorityType = NULL, ScheduledTaskID = NULL, ScheduleIsAvailableDaysBefore = NULL, SchoolYearID = NULL, SendNotificationForDay = NULL, SendNotificationForPriorDayCount = NULL, SendOnlyIfGuardianNotNotified = NULL, SendToDisciplineOfficer = NULL, SubjectCode = NULL, ToWhom = NULL, Type = NULL, UnrecordedAttendanceMinutes = NULL, UnrecordedAttendancePeriodType = NULL, XMLFilter = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "Notification", body = list(DataObject = body), searchFields = append("NotificationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Notification
	#'
	#' This function modifies a Notification
	#' @param fieldNames The field values to give the modified Notification. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The modified Notification
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNotification <- function(NotificationID, AttendanceCategoryForCount = NULL, AttendanceCountHigh = NULL, AttendanceCountLow = NULL, AttendanceCountMethod = NULL, ConsiderAllStaffMeets = NULL, DayType = NULL, EntityID = NULL, FeeManagementBalanceHigh = NULL, FeeManagementBalanceLow = NULL, FoodServiceBalanceHigh = NULL, FoodServiceBalanceLow = NULL, GradingPeriodEndDaysAfter = NULL, GradingPeriodEndDaysBefore = NULL, IncludeAutoGeneratedMessage = NULL, LastEntryType = NULL, Message = NULL, NumberOfDays = NULL, PriorityType = NULL, ScheduledTaskID = NULL, ScheduleIsAvailableDaysBefore = NULL, SchoolYearID = NULL, SendNotificationForDay = NULL, SendNotificationForPriorDayCount = NULL, SendOnlyIfGuardianNotNotified = NULL, SendToDisciplineOfficer = NULL, SubjectCode = NULL, ToWhom = NULL, Type = NULL, UnrecordedAttendanceMinutes = NULL, UnrecordedAttendancePeriodType = NULL, XMLFilter = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "Notification", objectId = NotificationID, body = list(DataObject = body), searchFields = append("NotificationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List UserMessageSettings
	#'
	#' This function returns a dataframe or json object of UserMessageSettings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given UserMessageSettings. Defaults to FALSE for all return fields which, for convenience, returns all fields for the UserMessageSettings.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('UserMessageSetting') to get more field paths.
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
	#' @concept MessageCenter
	#' @return A list of UserMessageSettings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listUserMessageSettings <- function(searchConditionsList = NULL, AssignmentScoreHighNotification = F, AssignmentScoreHighNotificationEmail = F, AssignmentScoreLowNotification = F, AssignmentScoreLowNotificationEmail = F, CopyAttendanceMessagesToEmail = F, CopyDisciplineMessagesToEmail = F, CopyEnrollmentMessagesToEmail = F, CopyFamilyAccessMessagesToEmail = F, CopyFeeManagementMessagesToEmail = F, CopyFoodServiceMessagesToEmail = F, CopyGradebookMessagesToEmail = F, CopyGradingMessagesToEmail = F, CopyGraduationRequirementsMessagesToEmail = F, CopyMessagesToEmail = F, CopyMTSSMessagesToEmail = F, CopyOnlineFormMessagesToEmail = F, CopyReportingMessagesToEmail = F, CopySchedulingMessagesToEmail = F, CreatedTime = F, CurrentGradeScoreHighNotification = F, CurrentGradeScoreHighNotificationEmail = F, CurrentGradeScoreLowNotification = F, CurrentGradeScoreLowNotificationEmail = F, EnableCompletedCareerPlanChangeNotification = F, EnableCompletedCareerPlanChangeNotificationEmail = F, EnableCompletedGradeChangeNotification = F, EnableCompletedGradeChangeNotificationEmail = F, EnableGradebookGradeChangeRequestDeniedEmail = F, EnableGradebookGradeChangeRequestNotificationEmail = F, EnableGradebookLastEntryNotificationEmail = F, EnableOnlineAssignmentAvailableNotificationEmail = F, EnableOnlineAssingmentScoresAvailableNotificationEmail = F, EnableStudentScheduleChangeNotification = F, EnableStudentScheduleChangeNotificationEmail = F, GradebookHighAssignmentThreshold = F, GradebookHighThreshold = F, GradebookLowAssignmentThreshold = F, GradebookLowThreshold = F, MissingAssignmentNotification = F, MissingAssignmentNotificationEmail = F, ModifiedTime = F, OnlySendAssignmentScoreHighNotificationsOncePerAssignment = F, OnlySendAssignmentScoreLowNotificationsOncePerAssignment = F, OnlySendCurrentGradeScoreHighNotificationsOnce = F, OnlySendCurrentGradeScoreLowNotificationsOnce = F, OnlySendMissingAssignmentNotificationForCurrentGradingPeriod = F, OnlySendMissingAssignmentNotificationsOncePerAssignment = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserIDOwner = F, UserMessageSettingID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "UserMessageSetting", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an UserMessageSetting
	#'
	#' This function returns a dataframe or json object of an UserMessageSetting
	#' @param UserMessageSettingID The ID of the UserMessageSetting to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given UserMessageSetting. Defaults to FALSE for all return fields which, for convenience, returns all fields for the UserMessageSetting.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('UserMessageSetting') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A dataframe or of UserMessageSetting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getUserMessageSetting <- function(UserMessageSettingID, AssignmentScoreHighNotification = F, AssignmentScoreHighNotificationEmail = F, AssignmentScoreLowNotification = F, AssignmentScoreLowNotificationEmail = F, CopyAttendanceMessagesToEmail = F, CopyDisciplineMessagesToEmail = F, CopyEnrollmentMessagesToEmail = F, CopyFamilyAccessMessagesToEmail = F, CopyFeeManagementMessagesToEmail = F, CopyFoodServiceMessagesToEmail = F, CopyGradebookMessagesToEmail = F, CopyGradingMessagesToEmail = F, CopyGraduationRequirementsMessagesToEmail = F, CopyMessagesToEmail = F, CopyMTSSMessagesToEmail = F, CopyOnlineFormMessagesToEmail = F, CopyReportingMessagesToEmail = F, CopySchedulingMessagesToEmail = F, CreatedTime = F, CurrentGradeScoreHighNotification = F, CurrentGradeScoreHighNotificationEmail = F, CurrentGradeScoreLowNotification = F, CurrentGradeScoreLowNotificationEmail = F, EnableCompletedCareerPlanChangeNotification = F, EnableCompletedCareerPlanChangeNotificationEmail = F, EnableCompletedGradeChangeNotification = F, EnableCompletedGradeChangeNotificationEmail = F, EnableGradebookGradeChangeRequestDeniedEmail = F, EnableGradebookGradeChangeRequestNotificationEmail = F, EnableGradebookLastEntryNotificationEmail = F, EnableOnlineAssignmentAvailableNotificationEmail = F, EnableOnlineAssingmentScoresAvailableNotificationEmail = F, EnableStudentScheduleChangeNotification = F, EnableStudentScheduleChangeNotificationEmail = F, GradebookHighAssignmentThreshold = F, GradebookHighThreshold = F, GradebookLowAssignmentThreshold = F, GradebookLowThreshold = F, MissingAssignmentNotification = F, MissingAssignmentNotificationEmail = F, ModifiedTime = F, OnlySendAssignmentScoreHighNotificationsOncePerAssignment = F, OnlySendAssignmentScoreLowNotificationsOncePerAssignment = F, OnlySendCurrentGradeScoreHighNotificationsOnce = F, OnlySendCurrentGradeScoreLowNotificationsOnce = F, OnlySendMissingAssignmentNotificationForCurrentGradingPeriod = F, OnlySendMissingAssignmentNotificationsOncePerAssignment = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserIDOwner = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "UserMessageSettingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "UserMessageSetting", objectId = UserMessageSettingID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an UserMessageSetting
	#'
	#' This function deletes an UserMessageSetting
	#' @param UserMessageSettingID The ID of the UserMessageSetting to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The UserMessageSettingID of the deleted UserMessageSetting.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteUserMessageSetting <- function(UserMessageSettingID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "UserMessageSetting", objectId = UserMessageSettingID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an UserMessageSetting
	#'
	#' This function creates an UserMessageSetting
	#' @param fieldNames The field values to give the created UserMessageSetting. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A newly created UserMessageSetting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createUserMessageSetting <- function(AssignmentScoreHighNotification = NULL, AssignmentScoreHighNotificationEmail = NULL, AssignmentScoreLowNotification = NULL, AssignmentScoreLowNotificationEmail = NULL, CopyAttendanceMessagesToEmail = NULL, CopyDisciplineMessagesToEmail = NULL, CopyEnrollmentMessagesToEmail = NULL, CopyFamilyAccessMessagesToEmail = NULL, CopyFeeManagementMessagesToEmail = NULL, CopyFoodServiceMessagesToEmail = NULL, CopyGradebookMessagesToEmail = NULL, CopyGradingMessagesToEmail = NULL, CopyGraduationRequirementsMessagesToEmail = NULL, CopyMessagesToEmail = NULL, CopyMTSSMessagesToEmail = NULL, CopyOnlineFormMessagesToEmail = NULL, CopyReportingMessagesToEmail = NULL, CopySchedulingMessagesToEmail = NULL, CurrentGradeScoreHighNotification = NULL, CurrentGradeScoreHighNotificationEmail = NULL, CurrentGradeScoreLowNotification = NULL, CurrentGradeScoreLowNotificationEmail = NULL, EnableCompletedCareerPlanChangeNotification = NULL, EnableCompletedCareerPlanChangeNotificationEmail = NULL, EnableCompletedGradeChangeNotification = NULL, EnableCompletedGradeChangeNotificationEmail = NULL, EnableGradebookGradeChangeRequestDeniedEmail = NULL, EnableGradebookGradeChangeRequestNotificationEmail = NULL, EnableGradebookLastEntryNotificationEmail = NULL, EnableOnlineAssignmentAvailableNotificationEmail = NULL, EnableOnlineAssingmentScoresAvailableNotificationEmail = NULL, EnableStudentScheduleChangeNotification = NULL, EnableStudentScheduleChangeNotificationEmail = NULL, GradebookHighAssignmentThreshold = NULL, GradebookHighThreshold = NULL, GradebookLowAssignmentThreshold = NULL, GradebookLowThreshold = NULL, MissingAssignmentNotification = NULL, MissingAssignmentNotificationEmail = NULL, OnlySendAssignmentScoreHighNotificationsOncePerAssignment = NULL, OnlySendAssignmentScoreLowNotificationsOncePerAssignment = NULL, OnlySendCurrentGradeScoreHighNotificationsOnce = NULL, OnlySendCurrentGradeScoreLowNotificationsOnce = NULL, OnlySendMissingAssignmentNotificationForCurrentGradingPeriod = NULL, OnlySendMissingAssignmentNotificationsOncePerAssignment = NULL, UserIDOwner = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "UserMessageSetting", body = list(DataObject = body), searchFields = append("UserMessageSettingID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an UserMessageSetting
	#'
	#' This function modifies an UserMessageSetting
	#' @param fieldNames The field values to give the modified UserMessageSetting. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The modified UserMessageSetting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyUserMessageSetting <- function(UserMessageSettingID, AssignmentScoreHighNotification = NULL, AssignmentScoreHighNotificationEmail = NULL, AssignmentScoreLowNotification = NULL, AssignmentScoreLowNotificationEmail = NULL, CopyAttendanceMessagesToEmail = NULL, CopyDisciplineMessagesToEmail = NULL, CopyEnrollmentMessagesToEmail = NULL, CopyFamilyAccessMessagesToEmail = NULL, CopyFeeManagementMessagesToEmail = NULL, CopyFoodServiceMessagesToEmail = NULL, CopyGradebookMessagesToEmail = NULL, CopyGradingMessagesToEmail = NULL, CopyGraduationRequirementsMessagesToEmail = NULL, CopyMessagesToEmail = NULL, CopyMTSSMessagesToEmail = NULL, CopyOnlineFormMessagesToEmail = NULL, CopyReportingMessagesToEmail = NULL, CopySchedulingMessagesToEmail = NULL, CurrentGradeScoreHighNotification = NULL, CurrentGradeScoreHighNotificationEmail = NULL, CurrentGradeScoreLowNotification = NULL, CurrentGradeScoreLowNotificationEmail = NULL, EnableCompletedCareerPlanChangeNotification = NULL, EnableCompletedCareerPlanChangeNotificationEmail = NULL, EnableCompletedGradeChangeNotification = NULL, EnableCompletedGradeChangeNotificationEmail = NULL, EnableGradebookGradeChangeRequestDeniedEmail = NULL, EnableGradebookGradeChangeRequestNotificationEmail = NULL, EnableGradebookLastEntryNotificationEmail = NULL, EnableOnlineAssignmentAvailableNotificationEmail = NULL, EnableOnlineAssingmentScoresAvailableNotificationEmail = NULL, EnableStudentScheduleChangeNotification = NULL, EnableStudentScheduleChangeNotificationEmail = NULL, GradebookHighAssignmentThreshold = NULL, GradebookHighThreshold = NULL, GradebookLowAssignmentThreshold = NULL, GradebookLowThreshold = NULL, MissingAssignmentNotification = NULL, MissingAssignmentNotificationEmail = NULL, OnlySendAssignmentScoreHighNotificationsOncePerAssignment = NULL, OnlySendAssignmentScoreLowNotificationsOncePerAssignment = NULL, OnlySendCurrentGradeScoreHighNotificationsOnce = NULL, OnlySendCurrentGradeScoreLowNotificationsOnce = NULL, OnlySendMissingAssignmentNotificationForCurrentGradingPeriod = NULL, OnlySendMissingAssignmentNotificationsOncePerAssignment = NULL, UserIDOwner = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "UserMessageSetting", objectId = UserMessageSettingID, body = list(DataObject = body), searchFields = append("UserMessageSettingID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Messages
	#'
	#' This function returns a dataframe or json object of Messages
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Messages. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Messages.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Message') to get more field paths.
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
	#' @concept MessageCenter
	#' @return A list of Messages
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listMessages <- function(searchConditionsList = NULL, Content = F, CreatedTime = F, EmailRecipientTypeCode = F, EmailsSent = F, FromInformation = F, IsHidden = F, IsRead = F, MessageID = F, MessageIDCopiedFrom = F, MessageMasterID = F, ModifiedTime = F, ObjectIDCreatedFor = F, ObjectPrimaryKey = F, PostTime = F, PriorityType = F, SourceIDCreatedFor = F, SubjectCode = F, Type = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserIDRecipient = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "Message", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Message
	#'
	#' This function returns a dataframe or json object of a Message
	#' @param MessageID The ID of the Message to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Message. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Message.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Message') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A dataframe or of Message
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getMessage <- function(MessageID, Content = F, CreatedTime = F, EmailRecipientTypeCode = F, EmailsSent = F, FromInformation = F, IsHidden = F, IsRead = F, MessageIDCopiedFrom = F, MessageMasterID = F, ModifiedTime = F, ObjectIDCreatedFor = F, ObjectPrimaryKey = F, PostTime = F, PriorityType = F, SourceIDCreatedFor = F, SubjectCode = F, Type = F, UserIDCreatedBy = F, UserIDModifiedBy = F, UserIDRecipient = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "MessageID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "Message", objectId = MessageID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Message
	#'
	#' This function deletes a Message
	#' @param MessageID The ID of the Message to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The MessageID of the deleted Message.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteMessage <- function(MessageID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "Message", objectId = MessageID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Message
	#'
	#' This function creates a Message
	#' @param fieldNames The field values to give the created Message. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A newly created Message
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createMessage <- function(Content = NULL, EmailRecipientTypeCode = NULL, FromInformation = NULL, IsHidden = NULL, IsRead = NULL, MessageIDCopiedFrom = NULL, MessageMasterID = NULL, ObjectIDCreatedFor = NULL, ObjectPrimaryKey = NULL, PriorityType = NULL, SourceIDCreatedFor = NULL, SubjectCode = NULL, Type = NULL, UserIDRecipient = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "Message", body = list(DataObject = body), searchFields = append("MessageID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Message
	#'
	#' This function modifies a Message
	#' @param fieldNames The field values to give the modified Message. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The modified Message
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyMessage <- function(MessageID, Content = NULL, EmailRecipientTypeCode = NULL, FromInformation = NULL, IsHidden = NULL, IsRead = NULL, MessageIDCopiedFrom = NULL, MessageMasterID = NULL, ObjectIDCreatedFor = NULL, ObjectPrimaryKey = NULL, PriorityType = NULL, SourceIDCreatedFor = NULL, SubjectCode = NULL, Type = NULL, UserIDRecipient = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "Message", objectId = MessageID, body = list(DataObject = body), searchFields = append("MessageID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List MessageMasters
	#'
	#' This function returns a dataframe or json object of MessageMasters
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given MessageMasters. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MessageMasters.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MessageMaster') to get more field paths.
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
	#' @concept MessageCenter
	#' @return A list of MessageMasters
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listMessageMasters <- function(searchConditionsList = NULL, AttachmentCount = F, Content = F, CreatedTime = F, EntityID = F, IncludeRestrictedGuardians = F, IsDraft = F, IsRetracted = F, LargestMessagePrimaryKey = F, MessageMasterID = F, ModifiedTime = F, NotificationID = F, PostedTime = F, PriorityType = F, SchoolYearID = F, SourceIDCreatedFor = F, Status = F, SubjectCleaned = F, SubjectCode = F, ToWhom = F, Type = F, UserIDCreatedBy = F, UserIDModifiedBy = F, XMLFilter = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "MessageMaster", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a MessageMaster
	#'
	#' This function returns a dataframe or json object of a MessageMaster
	#' @param MessageMasterID The ID of the MessageMaster to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given MessageMaster. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MessageMaster.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MessageMaster') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A dataframe or of MessageMaster
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getMessageMaster <- function(MessageMasterID, AttachmentCount = F, Content = F, CreatedTime = F, EntityID = F, IncludeRestrictedGuardians = F, IsDraft = F, IsRetracted = F, LargestMessagePrimaryKey = F, ModifiedTime = F, NotificationID = F, PostedTime = F, PriorityType = F, SchoolYearID = F, SourceIDCreatedFor = F, Status = F, SubjectCleaned = F, SubjectCode = F, ToWhom = F, Type = F, UserIDCreatedBy = F, UserIDModifiedBy = F, XMLFilter = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "MessageMasterID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "MessageMaster", objectId = MessageMasterID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a MessageMaster
	#'
	#' This function deletes a MessageMaster
	#' @param MessageMasterID The ID of the MessageMaster to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The MessageMasterID of the deleted MessageMaster.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteMessageMaster <- function(MessageMasterID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "MessageMaster", objectId = MessageMasterID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a MessageMaster
	#'
	#' This function creates a MessageMaster
	#' @param fieldNames The field values to give the created MessageMaster. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A newly created MessageMaster
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createMessageMaster <- function(Content = NULL, EntityID = NULL, IncludeRestrictedGuardians = NULL, IsDraft = NULL, NotificationID = NULL, PostedTime = NULL, PriorityType = NULL, SchoolYearID = NULL, SourceIDCreatedFor = NULL, Status = NULL, SubjectCode = NULL, ToWhom = NULL, Type = NULL, XMLFilter = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "MessageMaster", body = list(DataObject = body), searchFields = append("MessageMasterID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a MessageMaster
	#'
	#' This function modifies a MessageMaster
	#' @param fieldNames The field values to give the modified MessageMaster. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The modified MessageMaster
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyMessageMaster <- function(MessageMasterID, Content = NULL, EntityID = NULL, IncludeRestrictedGuardians = NULL, IsDraft = NULL, NotificationID = NULL, PostedTime = NULL, PriorityType = NULL, SchoolYearID = NULL, SourceIDCreatedFor = NULL, Status = NULL, SubjectCode = NULL, ToWhom = NULL, Type = NULL, XMLFilter = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "MessageMaster", objectId = MessageMasterID, body = list(DataObject = body), searchFields = append("MessageMasterID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List NotificationWithdrawalCodes
	#'
	#' This function returns a dataframe or json object of NotificationWithdrawalCodes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NotificationWithdrawalCodes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NotificationWithdrawalCodes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NotificationWithdrawalCode') to get more field paths.
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
	#' @concept MessageCenter
	#' @return A list of NotificationWithdrawalCodes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNotificationWithdrawalCodes <- function(searchConditionsList = NULL, CreatedTime = F, ModifiedTime = F, NotificationID = F, NotificationWithdrawalCodeID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, WithdrawalCodeID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "NotificationWithdrawalCode", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NotificationWithdrawalCode
	#'
	#' This function returns a dataframe or json object of a NotificationWithdrawalCode
	#' @param NotificationWithdrawalCodeID The ID of the NotificationWithdrawalCode to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NotificationWithdrawalCode. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NotificationWithdrawalCode.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NotificationWithdrawalCode') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A dataframe or of NotificationWithdrawalCode
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNotificationWithdrawalCode <- function(NotificationWithdrawalCodeID, CreatedTime = F, ModifiedTime = F, NotificationID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, WithdrawalCodeID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NotificationWithdrawalCodeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "NotificationWithdrawalCode", objectId = NotificationWithdrawalCodeID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NotificationWithdrawalCode
	#'
	#' This function deletes a NotificationWithdrawalCode
	#' @param NotificationWithdrawalCodeID The ID of the NotificationWithdrawalCode to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The NotificationWithdrawalCodeID of the deleted NotificationWithdrawalCode.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNotificationWithdrawalCode <- function(NotificationWithdrawalCodeID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "NotificationWithdrawalCode", objectId = NotificationWithdrawalCodeID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NotificationWithdrawalCode
	#'
	#' This function creates a NotificationWithdrawalCode
	#' @param fieldNames The field values to give the created NotificationWithdrawalCode. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A newly created NotificationWithdrawalCode
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNotificationWithdrawalCode <- function(NotificationID = NULL, WithdrawalCodeID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "NotificationWithdrawalCode", body = list(DataObject = body), searchFields = append("NotificationWithdrawalCodeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NotificationWithdrawalCode
	#'
	#' This function modifies a NotificationWithdrawalCode
	#' @param fieldNames The field values to give the modified NotificationWithdrawalCode. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The modified NotificationWithdrawalCode
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNotificationWithdrawalCode <- function(NotificationWithdrawalCodeID, NotificationID = NULL, WithdrawalCodeID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "NotificationWithdrawalCode", objectId = NotificationWithdrawalCodeID, body = list(DataObject = body), searchFields = append("NotificationWithdrawalCodeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List QueuedCompletedCareerPlanChangeNotifications
	#'
	#' This function returns a dataframe or json object of QueuedCompletedCareerPlanChangeNotifications
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given QueuedCompletedCareerPlanChangeNotifications. Defaults to FALSE for all return fields which, for convenience, returns all fields for the QueuedCompletedCareerPlanChangeNotifications.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('QueuedCompletedCareerPlanChangeNotification') to get more field paths.
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
	#' @concept MessageCenter
	#' @return A list of QueuedCompletedCareerPlanChangeNotifications
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listQueuedCompletedCareerPlanChangeNotifications <- function(searchConditionsList = NULL, CareerPlanGradeLevelIDCurrent = F, CareerPlanGradeLevelIDPrevious = F, CreatedTime = F, CreditsCurrent = F, CreditsPrevious = F, CurriculumID = F, EntityID = F, IsDeletedRecord = F, IsNewRecord = F, IsSent = F, IsStudentPermittedToChangeGradeLevelCurrent = F, IsStudentPermittedToChangeGradeLevelPrevious = F, IsStudentPermittedToDeleteCurrent = F, IsStudentPermittedToDeletePrevious = F, ModifiedTime = F, NotificationID = F, QueuedCompletedCareerPlanChangeNotificationID = F, SchoolYearID = F, StudentCareerPlanID = F, StudentCourseRequestIDCurrent = F, StudentCourseRequestIDPrevious = F, StudentID = F, StudentSubAreaIDCurrent = F, StudentSubAreaIDPrevious = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "QueuedCompletedCareerPlanChangeNotification", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a QueuedCompletedCareerPlanChangeNotification
	#'
	#' This function returns a dataframe or json object of a QueuedCompletedCareerPlanChangeNotification
	#' @param QueuedCompletedCareerPlanChangeNotificationID The ID of the QueuedCompletedCareerPlanChangeNotification to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given QueuedCompletedCareerPlanChangeNotification. Defaults to FALSE for all return fields which, for convenience, returns all fields for the QueuedCompletedCareerPlanChangeNotification.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('QueuedCompletedCareerPlanChangeNotification') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A dataframe or of QueuedCompletedCareerPlanChangeNotification
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getQueuedCompletedCareerPlanChangeNotification <- function(QueuedCompletedCareerPlanChangeNotificationID, CareerPlanGradeLevelIDCurrent = F, CareerPlanGradeLevelIDPrevious = F, CreatedTime = F, CreditsCurrent = F, CreditsPrevious = F, CurriculumID = F, EntityID = F, IsDeletedRecord = F, IsNewRecord = F, IsSent = F, IsStudentPermittedToChangeGradeLevelCurrent = F, IsStudentPermittedToChangeGradeLevelPrevious = F, IsStudentPermittedToDeleteCurrent = F, IsStudentPermittedToDeletePrevious = F, ModifiedTime = F, NotificationID = F, SchoolYearID = F, StudentCareerPlanID = F, StudentCourseRequestIDCurrent = F, StudentCourseRequestIDPrevious = F, StudentID = F, StudentSubAreaIDCurrent = F, StudentSubAreaIDPrevious = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "QueuedCompletedCareerPlanChangeNotificationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "QueuedCompletedCareerPlanChangeNotification", objectId = QueuedCompletedCareerPlanChangeNotificationID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a QueuedCompletedCareerPlanChangeNotification
	#'
	#' This function deletes a QueuedCompletedCareerPlanChangeNotification
	#' @param QueuedCompletedCareerPlanChangeNotificationID The ID of the QueuedCompletedCareerPlanChangeNotification to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The QueuedCompletedCareerPlanChangeNotificationID of the deleted QueuedCompletedCareerPlanChangeNotification.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteQueuedCompletedCareerPlanChangeNotification <- function(QueuedCompletedCareerPlanChangeNotificationID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "QueuedCompletedCareerPlanChangeNotification", objectId = QueuedCompletedCareerPlanChangeNotificationID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a QueuedCompletedCareerPlanChangeNotification
	#'
	#' This function creates a QueuedCompletedCareerPlanChangeNotification
	#' @param fieldNames The field values to give the created QueuedCompletedCareerPlanChangeNotification. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A newly created QueuedCompletedCareerPlanChangeNotification
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createQueuedCompletedCareerPlanChangeNotification <- function(CareerPlanGradeLevelIDCurrent = NULL, CareerPlanGradeLevelIDPrevious = NULL, CreditsCurrent = NULL, CreditsPrevious = NULL, CurriculumID = NULL, EntityID = NULL, IsDeletedRecord = NULL, IsNewRecord = NULL, IsSent = NULL, IsStudentPermittedToChangeGradeLevelCurrent = NULL, IsStudentPermittedToChangeGradeLevelPrevious = NULL, IsStudentPermittedToDeleteCurrent = NULL, IsStudentPermittedToDeletePrevious = NULL, NotificationID = NULL, SchoolYearID = NULL, StudentCareerPlanID = NULL, StudentCourseRequestIDCurrent = NULL, StudentCourseRequestIDPrevious = NULL, StudentID = NULL, StudentSubAreaIDCurrent = NULL, StudentSubAreaIDPrevious = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "QueuedCompletedCareerPlanChangeNotification", body = list(DataObject = body), searchFields = append("QueuedCompletedCareerPlanChangeNotificationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a QueuedCompletedCareerPlanChangeNotification
	#'
	#' This function modifies a QueuedCompletedCareerPlanChangeNotification
	#' @param fieldNames The field values to give the modified QueuedCompletedCareerPlanChangeNotification. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The modified QueuedCompletedCareerPlanChangeNotification
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyQueuedCompletedCareerPlanChangeNotification <- function(QueuedCompletedCareerPlanChangeNotificationID, CareerPlanGradeLevelIDCurrent = NULL, CareerPlanGradeLevelIDPrevious = NULL, CreditsCurrent = NULL, CreditsPrevious = NULL, CurriculumID = NULL, EntityID = NULL, IsDeletedRecord = NULL, IsNewRecord = NULL, IsSent = NULL, IsStudentPermittedToChangeGradeLevelCurrent = NULL, IsStudentPermittedToChangeGradeLevelPrevious = NULL, IsStudentPermittedToDeleteCurrent = NULL, IsStudentPermittedToDeletePrevious = NULL, NotificationID = NULL, SchoolYearID = NULL, StudentCareerPlanID = NULL, StudentCourseRequestIDCurrent = NULL, StudentCourseRequestIDPrevious = NULL, StudentID = NULL, StudentSubAreaIDCurrent = NULL, StudentSubAreaIDPrevious = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "QueuedCompletedCareerPlanChangeNotification", objectId = QueuedCompletedCareerPlanChangeNotificationID, body = list(DataObject = body), searchFields = append("QueuedCompletedCareerPlanChangeNotificationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List MessageCenterConfigDistrictYears
	#'
	#' This function returns a dataframe or json object of MessageCenterConfigDistrictYears
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given MessageCenterConfigDistrictYears. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MessageCenterConfigDistrictYears.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MessageCenterConfigDistrictYear') to get more field paths.
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
	#' @concept MessageCenter
	#' @return A list of MessageCenterConfigDistrictYears
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listMessageCenterConfigDistrictYears <- function(searchConditionsList = NULL, ConfigDistrictYearID = F, ConfigDistrictYearIDClonedFrom = F, CreatedTime = F, DistrictID = F, ModifiedTime = F, NumberOfDaysAfterWithdrawalToAllowMessages = F, SchoolYearID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "ConfigDistrictYear", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a MessageCenterConfigDistrictYear
	#'
	#' This function returns a dataframe or json object of a MessageCenterConfigDistrictYear
	#' @param MessageCenterConfigDistrictYearID The ID of the MessageCenterConfigDistrictYear to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given MessageCenterConfigDistrictYear. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MessageCenterConfigDistrictYear.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MessageCenterConfigDistrictYear') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A dataframe or of MessageCenterConfigDistrictYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getMessageCenterConfigDistrictYear <- function(MessageCenterConfigDistrictYearID, ConfigDistrictYearID = F, ConfigDistrictYearIDClonedFrom = F, CreatedTime = F, DistrictID = F, ModifiedTime = F, NumberOfDaysAfterWithdrawalToAllowMessages = F, SchoolYearID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "MessageCenterConfigDistrictYearID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "ConfigDistrictYear", objectId = MessageCenterConfigDistrictYearID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a MessageCenterConfigDistrictYear
	#'
	#' This function deletes a MessageCenterConfigDistrictYear
	#' @param MessageCenterConfigDistrictYearID The ID of the MessageCenterConfigDistrictYear to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The MessageCenterConfigDistrictYearID of the deleted MessageCenterConfigDistrictYear.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteMessageCenterConfigDistrictYear <- function(MessageCenterConfigDistrictYearID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "ConfigDistrictYear", objectId = MessageCenterConfigDistrictYearID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a MessageCenterConfigDistrictYear
	#'
	#' This function creates a MessageCenterConfigDistrictYear
	#' @param fieldNames The field values to give the created MessageCenterConfigDistrictYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A newly created MessageCenterConfigDistrictYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createMessageCenterConfigDistrictYear <- function(ConfigDistrictYearIDClonedFrom = NULL, DistrictID = NULL, NumberOfDaysAfterWithdrawalToAllowMessages = NULL, SchoolYearID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "ConfigDistrictYear", body = list(DataObject = body), searchFields = append("ConfigDistrictYearID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a MessageCenterConfigDistrictYear
	#'
	#' This function modifies a MessageCenterConfigDistrictYear
	#' @param fieldNames The field values to give the modified MessageCenterConfigDistrictYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The modified MessageCenterConfigDistrictYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyMessageCenterConfigDistrictYear <- function(ConfigDistrictYearID, ConfigDistrictYearIDClonedFrom = NULL, DistrictID = NULL, NumberOfDaysAfterWithdrawalToAllowMessages = NULL, SchoolYearID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "ConfigDistrictYear", objectId = ConfigDistrictYearID, body = list(DataObject = body), searchFields = append("ConfigDistrictYearID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List MessageCenterConfigDistrictYearWithdrawalCodes
	#'
	#' This function returns a dataframe or json object of MessageCenterConfigDistrictYearWithdrawalCodes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given MessageCenterConfigDistrictYearWithdrawalCodes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MessageCenterConfigDistrictYearWithdrawalCodes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MessageCenterConfigDistrictYearWithdrawalCode') to get more field paths.
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
	#' @concept MessageCenter
	#' @return A list of MessageCenterConfigDistrictYearWithdrawalCodes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listMessageCenterConfigDistrictYearWithdrawalCodes <- function(searchConditionsList = NULL, ConfigDistrictYearID = F, ConfigDistrictYearWithdrawalCodeID = F, ConfigDistrictYearWithdrawalCodeIDClonedFrom = F, CreatedTime = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, WithdrawalCodeID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "ConfigDistrictYearWithdrawalCode", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a MessageCenterConfigDistrictYearWithdrawalCode
	#'
	#' This function returns a dataframe or json object of a MessageCenterConfigDistrictYearWithdrawalCode
	#' @param MessageCenterConfigDistrictYearWithdrawalCodeID The ID of the MessageCenterConfigDistrictYearWithdrawalCode to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given MessageCenterConfigDistrictYearWithdrawalCode. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MessageCenterConfigDistrictYearWithdrawalCode.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MessageCenterConfigDistrictYearWithdrawalCode') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A dataframe or of MessageCenterConfigDistrictYearWithdrawalCode
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getMessageCenterConfigDistrictYearWithdrawalCode <- function(MessageCenterConfigDistrictYearWithdrawalCodeID, ConfigDistrictYearID = F, ConfigDistrictYearWithdrawalCodeID = F, ConfigDistrictYearWithdrawalCodeIDClonedFrom = F, CreatedTime = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, WithdrawalCodeID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "MessageCenterConfigDistrictYearWithdrawalCodeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "ConfigDistrictYearWithdrawalCode", objectId = MessageCenterConfigDistrictYearWithdrawalCodeID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a MessageCenterConfigDistrictYearWithdrawalCode
	#'
	#' This function deletes a MessageCenterConfigDistrictYearWithdrawalCode
	#' @param MessageCenterConfigDistrictYearWithdrawalCodeID The ID of the MessageCenterConfigDistrictYearWithdrawalCode to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The MessageCenterConfigDistrictYearWithdrawalCodeID of the deleted MessageCenterConfigDistrictYearWithdrawalCode.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteMessageCenterConfigDistrictYearWithdrawalCode <- function(MessageCenterConfigDistrictYearWithdrawalCodeID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "ConfigDistrictYearWithdrawalCode", objectId = MessageCenterConfigDistrictYearWithdrawalCodeID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a MessageCenterConfigDistrictYearWithdrawalCode
	#'
	#' This function creates a MessageCenterConfigDistrictYearWithdrawalCode
	#' @param fieldNames The field values to give the created MessageCenterConfigDistrictYearWithdrawalCode. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A newly created MessageCenterConfigDistrictYearWithdrawalCode
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createMessageCenterConfigDistrictYearWithdrawalCode <- function(ConfigDistrictYearID = NULL, ConfigDistrictYearWithdrawalCodeIDClonedFrom = NULL, WithdrawalCodeID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "ConfigDistrictYearWithdrawalCode", body = list(DataObject = body), searchFields = append("ConfigDistrictYearWithdrawalCodeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a MessageCenterConfigDistrictYearWithdrawalCode
	#'
	#' This function modifies a MessageCenterConfigDistrictYearWithdrawalCode
	#' @param fieldNames The field values to give the modified MessageCenterConfigDistrictYearWithdrawalCode. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The modified MessageCenterConfigDistrictYearWithdrawalCode
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyMessageCenterConfigDistrictYearWithdrawalCode <- function(ConfigDistrictYearWithdrawalCodeID, ConfigDistrictYearID = NULL, ConfigDistrictYearWithdrawalCodeIDClonedFrom = NULL, WithdrawalCodeID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "ConfigDistrictYearWithdrawalCode", objectId = ConfigDistrictYearWithdrawalCodeID, body = list(DataObject = body), searchFields = append("ConfigDistrictYearWithdrawalCodeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SystemEmailTypeConfigs
	#'
	#' This function returns a dataframe or json object of SystemEmailTypeConfigs
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SystemEmailTypeConfigs. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SystemEmailTypeConfigs.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SystemEmailTypeConfig') to get more field paths.
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
	#' @concept MessageCenter
	#' @return A list of SystemEmailTypeConfigs
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSystemEmailTypeConfigs <- function(searchConditionsList = NULL, CreatedTime = F, EmailRecipientType = F, EmailTypeID = F, ModifiedTime = F, Rank = F, SystemEmailTypeConfigID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "SystemEmailTypeConfig", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SystemEmailTypeConfig
	#'
	#' This function returns a dataframe or json object of a SystemEmailTypeConfig
	#' @param SystemEmailTypeConfigID The ID of the SystemEmailTypeConfig to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SystemEmailTypeConfig. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SystemEmailTypeConfig.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SystemEmailTypeConfig') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A dataframe or of SystemEmailTypeConfig
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSystemEmailTypeConfig <- function(SystemEmailTypeConfigID, CreatedTime = F, EmailRecipientType = F, EmailTypeID = F, ModifiedTime = F, Rank = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SystemEmailTypeConfigID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "SystemEmailTypeConfig", objectId = SystemEmailTypeConfigID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SystemEmailTypeConfig
	#'
	#' This function deletes a SystemEmailTypeConfig
	#' @param SystemEmailTypeConfigID The ID of the SystemEmailTypeConfig to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The SystemEmailTypeConfigID of the deleted SystemEmailTypeConfig.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSystemEmailTypeConfig <- function(SystemEmailTypeConfigID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "SystemEmailTypeConfig", objectId = SystemEmailTypeConfigID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SystemEmailTypeConfig
	#'
	#' This function creates a SystemEmailTypeConfig
	#' @param fieldNames The field values to give the created SystemEmailTypeConfig. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return A newly created SystemEmailTypeConfig
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSystemEmailTypeConfig <- function(EmailRecipientType = NULL, EmailTypeID = NULL, Rank = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "SystemEmailTypeConfig", body = list(DataObject = body), searchFields = append("SystemEmailTypeConfigID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SystemEmailTypeConfig
	#'
	#' This function modifies a SystemEmailTypeConfig
	#' @param fieldNames The field values to give the modified SystemEmailTypeConfig. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MessageCenter
	#' @return The modified SystemEmailTypeConfig
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySystemEmailTypeConfig <- function(SystemEmailTypeConfigID, EmailRecipientType = NULL, EmailTypeID = NULL, Rank = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "SystemEmailTypeConfig", objectId = SystemEmailTypeConfigID, body = list(DataObject = body), searchFields = append("SystemEmailTypeConfigID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}
