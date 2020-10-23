
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A list of QueuedCompletedGradeChangeNotifications
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listQueuedCompletedGradeChangeNotifications <- function(searchConditionsList = NULL, QueuedCompletedGradeChangeNotificationID = F, NotificationID = F, StudentGradeBucketID = F, GradeMarkIDPrevious = F, GradeMarkIDCurrent = F, EntityID = F, SchoolYearID = F, IsSent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "QueuedCompletedGradeChangeNotification", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a QueuedCompletedGradeChangeNotification
	#'
	#' This function returns a dataframe or json object of a QueuedCompletedGradeChangeNotification
	#' @param QueuedCompletedGradeChangeNotificationID The ID of the QueuedCompletedGradeChangeNotification to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given QueuedCompletedGradeChangeNotification. Defaults to FALSE for all return fields which, for convenience, returns all fields for the QueuedCompletedGradeChangeNotification.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('QueuedCompletedGradeChangeNotification') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A dataframe or of QueuedCompletedGradeChangeNotification
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getQueuedCompletedGradeChangeNotification <- function(QueuedCompletedGradeChangeNotificationID, NotificationID = F, StudentGradeBucketID = F, GradeMarkIDPrevious = F, GradeMarkIDCurrent = F, EntityID = F, SchoolYearID = F, IsSent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "QueuedCompletedGradeChangeNotificationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "QueuedCompletedGradeChangeNotification", objectId = QueuedCompletedGradeChangeNotificationID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a QueuedCompletedGradeChangeNotification
	#'
	#' This function deletes a QueuedCompletedGradeChangeNotification
	#' @param QueuedCompletedGradeChangeNotificationID The ID of the QueuedCompletedGradeChangeNotification to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The QueuedCompletedGradeChangeNotificationID of the deleted QueuedCompletedGradeChangeNotification.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteQueuedCompletedGradeChangeNotification <- function(QueuedCompletedGradeChangeNotificationID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "QueuedCompletedGradeChangeNotification", objectId = QueuedCompletedGradeChangeNotificationID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a QueuedCompletedGradeChangeNotification
	#'
	#' This function creates a QueuedCompletedGradeChangeNotification
	#' @param fieldNames The field values to give the created QueuedCompletedGradeChangeNotification. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A newly created QueuedCompletedGradeChangeNotification
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createQueuedCompletedGradeChangeNotification <- function(NotificationID = NULL, StudentGradeBucketID = NULL, GradeMarkIDPrevious = NULL, GradeMarkIDCurrent = NULL, EntityID = NULL, SchoolYearID = NULL, IsSent = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "QueuedCompletedGradeChangeNotification", body = list(DataObject = body), searchFields = append("QueuedCompletedGradeChangeNotificationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a QueuedCompletedGradeChangeNotification
	#'
	#' This function modifies a QueuedCompletedGradeChangeNotification
	#' @param fieldNames The field values to give the modified QueuedCompletedGradeChangeNotification. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The modified QueuedCompletedGradeChangeNotification
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyQueuedCompletedGradeChangeNotification <- function(QueuedCompletedGradeChangeNotificationID, NotificationID = NULL, StudentGradeBucketID = NULL, GradeMarkIDPrevious = NULL, GradeMarkIDCurrent = NULL, EntityID = NULL, SchoolYearID = NULL, IsSent = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "QueuedCompletedGradeChangeNotification", objectId = QueuedCompletedGradeChangeNotificationID, body = list(DataObject = body), searchFields = append("QueuedCompletedGradeChangeNotificationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A list of NotificationDisciplineThresholds
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNotificationDisciplineThresholds <- function(searchConditionsList = NULL, NotificationDisciplineThresholdID = F, NotificationID = F, DisciplineThresholdID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "NotificationDisciplineThreshold", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NotificationDisciplineThreshold
	#'
	#' This function returns a dataframe or json object of a NotificationDisciplineThreshold
	#' @param NotificationDisciplineThresholdID The ID of the NotificationDisciplineThreshold to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NotificationDisciplineThreshold. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NotificationDisciplineThreshold.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NotificationDisciplineThreshold') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A dataframe or of NotificationDisciplineThreshold
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNotificationDisciplineThreshold <- function(NotificationDisciplineThresholdID, NotificationID = F, DisciplineThresholdID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NotificationDisciplineThresholdID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "NotificationDisciplineThreshold", objectId = NotificationDisciplineThresholdID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NotificationDisciplineThreshold
	#'
	#' This function deletes a NotificationDisciplineThreshold
	#' @param NotificationDisciplineThresholdID The ID of the NotificationDisciplineThreshold to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The NotificationDisciplineThresholdID of the deleted NotificationDisciplineThreshold.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNotificationDisciplineThreshold <- function(NotificationDisciplineThresholdID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "NotificationDisciplineThreshold", objectId = NotificationDisciplineThresholdID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NotificationDisciplineThreshold
	#'
	#' This function creates a NotificationDisciplineThreshold
	#' @param fieldNames The field values to give the created NotificationDisciplineThreshold. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A newly created NotificationDisciplineThreshold
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNotificationDisciplineThreshold <- function(NotificationID = NULL, DisciplineThresholdID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "NotificationDisciplineThreshold", body = list(DataObject = body), searchFields = append("NotificationDisciplineThresholdID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NotificationDisciplineThreshold
	#'
	#' This function modifies a NotificationDisciplineThreshold
	#' @param fieldNames The field values to give the modified NotificationDisciplineThreshold. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The modified NotificationDisciplineThreshold
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNotificationDisciplineThreshold <- function(NotificationDisciplineThresholdID, NotificationID = NULL, DisciplineThresholdID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "NotificationDisciplineThreshold", objectId = NotificationDisciplineThresholdID, body = list(DataObject = body), searchFields = append("NotificationDisciplineThresholdID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A list of NotificationGradeReferences
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNotificationGradeReferences <- function(searchConditionsList = NULL, NotificationGradeReferenceID = F, NotificationID = F, GradeReferenceID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "NotificationGradeReference", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NotificationGradeReference
	#'
	#' This function returns a dataframe or json object of a NotificationGradeReference
	#' @param NotificationGradeReferenceID The ID of the NotificationGradeReference to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NotificationGradeReference. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NotificationGradeReference.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NotificationGradeReference') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A dataframe or of NotificationGradeReference
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNotificationGradeReference <- function(NotificationGradeReferenceID, NotificationID = F, GradeReferenceID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NotificationGradeReferenceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "NotificationGradeReference", objectId = NotificationGradeReferenceID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NotificationGradeReference
	#'
	#' This function deletes a NotificationGradeReference
	#' @param NotificationGradeReferenceID The ID of the NotificationGradeReference to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The NotificationGradeReferenceID of the deleted NotificationGradeReference.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNotificationGradeReference <- function(NotificationGradeReferenceID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "NotificationGradeReference", objectId = NotificationGradeReferenceID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NotificationGradeReference
	#'
	#' This function creates a NotificationGradeReference
	#' @param fieldNames The field values to give the created NotificationGradeReference. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A newly created NotificationGradeReference
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNotificationGradeReference <- function(NotificationID = NULL, GradeReferenceID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "NotificationGradeReference", body = list(DataObject = body), searchFields = append("NotificationGradeReferenceID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NotificationGradeReference
	#'
	#' This function modifies a NotificationGradeReference
	#' @param fieldNames The field values to give the modified NotificationGradeReference. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The modified NotificationGradeReference
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNotificationGradeReference <- function(NotificationGradeReferenceID, NotificationID = NULL, GradeReferenceID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "NotificationGradeReference", objectId = NotificationGradeReferenceID, body = list(DataObject = body), searchFields = append("NotificationGradeReferenceID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A list of NotificationUnrecordedClassAttendances
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNotificationUnrecordedClassAttendances <- function(searchConditionsList = NULL, NotificationUnrecordedClassAttendanceID = F, NotificationID = F, DisplayPeriodID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "NotificationUnrecordedClassAttendance", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NotificationUnrecordedClassAttendance
	#'
	#' This function returns a dataframe or json object of a NotificationUnrecordedClassAttendance
	#' @param NotificationUnrecordedClassAttendanceID The ID of the NotificationUnrecordedClassAttendance to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NotificationUnrecordedClassAttendance. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NotificationUnrecordedClassAttendance.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NotificationUnrecordedClassAttendance') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A dataframe or of NotificationUnrecordedClassAttendance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNotificationUnrecordedClassAttendance <- function(NotificationUnrecordedClassAttendanceID, NotificationID = F, DisplayPeriodID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NotificationUnrecordedClassAttendanceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "NotificationUnrecordedClassAttendance", objectId = NotificationUnrecordedClassAttendanceID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NotificationUnrecordedClassAttendance
	#'
	#' This function deletes a NotificationUnrecordedClassAttendance
	#' @param NotificationUnrecordedClassAttendanceID The ID of the NotificationUnrecordedClassAttendance to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The NotificationUnrecordedClassAttendanceID of the deleted NotificationUnrecordedClassAttendance.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNotificationUnrecordedClassAttendance <- function(NotificationUnrecordedClassAttendanceID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "NotificationUnrecordedClassAttendance", objectId = NotificationUnrecordedClassAttendanceID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NotificationUnrecordedClassAttendance
	#'
	#' This function creates a NotificationUnrecordedClassAttendance
	#' @param fieldNames The field values to give the created NotificationUnrecordedClassAttendance. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A newly created NotificationUnrecordedClassAttendance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNotificationUnrecordedClassAttendance <- function(NotificationID = NULL, DisplayPeriodID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "NotificationUnrecordedClassAttendance", body = list(DataObject = body), searchFields = append("NotificationUnrecordedClassAttendanceID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NotificationUnrecordedClassAttendance
	#'
	#' This function modifies a NotificationUnrecordedClassAttendance
	#' @param fieldNames The field values to give the modified NotificationUnrecordedClassAttendance. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The modified NotificationUnrecordedClassAttendance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNotificationUnrecordedClassAttendance <- function(NotificationUnrecordedClassAttendanceID, NotificationID = NULL, DisplayPeriodID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "NotificationUnrecordedClassAttendance", objectId = NotificationUnrecordedClassAttendanceID, body = list(DataObject = body), searchFields = append("NotificationUnrecordedClassAttendanceID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A list of NotificationScheduleIsAvailableSectionLengths
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNotificationScheduleIsAvailableSectionLengths <- function(searchConditionsList = NULL, NotificationScheduleIsAvailableSectionLengthID = F, NotificationID = F, SectionLengthID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "NotificationScheduleIsAvailableSectionLength", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NotificationScheduleIsAvailableSectionLength
	#'
	#' This function returns a dataframe or json object of a NotificationScheduleIsAvailableSectionLength
	#' @param NotificationScheduleIsAvailableSectionLengthID The ID of the NotificationScheduleIsAvailableSectionLength to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NotificationScheduleIsAvailableSectionLength. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NotificationScheduleIsAvailableSectionLength.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NotificationScheduleIsAvailableSectionLength') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A dataframe or of NotificationScheduleIsAvailableSectionLength
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNotificationScheduleIsAvailableSectionLength <- function(NotificationScheduleIsAvailableSectionLengthID, NotificationID = F, SectionLengthID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NotificationScheduleIsAvailableSectionLengthID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "NotificationScheduleIsAvailableSectionLength", objectId = NotificationScheduleIsAvailableSectionLengthID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NotificationScheduleIsAvailableSectionLength
	#'
	#' This function deletes a NotificationScheduleIsAvailableSectionLength
	#' @param NotificationScheduleIsAvailableSectionLengthID The ID of the NotificationScheduleIsAvailableSectionLength to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The NotificationScheduleIsAvailableSectionLengthID of the deleted NotificationScheduleIsAvailableSectionLength.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNotificationScheduleIsAvailableSectionLength <- function(NotificationScheduleIsAvailableSectionLengthID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "NotificationScheduleIsAvailableSectionLength", objectId = NotificationScheduleIsAvailableSectionLengthID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NotificationScheduleIsAvailableSectionLength
	#'
	#' This function creates a NotificationScheduleIsAvailableSectionLength
	#' @param fieldNames The field values to give the created NotificationScheduleIsAvailableSectionLength. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A newly created NotificationScheduleIsAvailableSectionLength
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNotificationScheduleIsAvailableSectionLength <- function(NotificationID = NULL, SectionLengthID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "NotificationScheduleIsAvailableSectionLength", body = list(DataObject = body), searchFields = append("NotificationScheduleIsAvailableSectionLengthID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NotificationScheduleIsAvailableSectionLength
	#'
	#' This function modifies a NotificationScheduleIsAvailableSectionLength
	#' @param fieldNames The field values to give the modified NotificationScheduleIsAvailableSectionLength. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The modified NotificationScheduleIsAvailableSectionLength
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNotificationScheduleIsAvailableSectionLength <- function(NotificationScheduleIsAvailableSectionLengthID, NotificationID = NULL, SectionLengthID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "NotificationScheduleIsAvailableSectionLength", objectId = NotificationScheduleIsAvailableSectionLengthID, body = list(DataObject = body), searchFields = append("NotificationScheduleIsAvailableSectionLengthID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A list of NotificationOnlineForms
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNotificationOnlineForms <- function(searchConditionsList = NULL, NotificationOnlineFormID = F, NotificationID = F, OnlineFormID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "NotificationOnlineForm", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NotificationOnlineForm
	#'
	#' This function returns a dataframe or json object of a NotificationOnlineForm
	#' @param NotificationOnlineFormID The ID of the NotificationOnlineForm to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NotificationOnlineForm. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NotificationOnlineForm.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NotificationOnlineForm') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A dataframe or of NotificationOnlineForm
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNotificationOnlineForm <- function(NotificationOnlineFormID, NotificationID = F, OnlineFormID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NotificationOnlineFormID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "NotificationOnlineForm", objectId = NotificationOnlineFormID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NotificationOnlineForm
	#'
	#' This function deletes a NotificationOnlineForm
	#' @param NotificationOnlineFormID The ID of the NotificationOnlineForm to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The NotificationOnlineFormID of the deleted NotificationOnlineForm.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNotificationOnlineForm <- function(NotificationOnlineFormID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "NotificationOnlineForm", objectId = NotificationOnlineFormID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NotificationOnlineForm
	#'
	#' This function creates a NotificationOnlineForm
	#' @param fieldNames The field values to give the created NotificationOnlineForm. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A newly created NotificationOnlineForm
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNotificationOnlineForm <- function(NotificationID = NULL, OnlineFormID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "NotificationOnlineForm", body = list(DataObject = body), searchFields = append("NotificationOnlineFormID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NotificationOnlineForm
	#'
	#' This function modifies a NotificationOnlineForm
	#' @param fieldNames The field values to give the modified NotificationOnlineForm. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The modified NotificationOnlineForm
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNotificationOnlineForm <- function(NotificationOnlineFormID, NotificationID = NULL, OnlineFormID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "NotificationOnlineForm", objectId = NotificationOnlineFormID, body = list(DataObject = body), searchFields = append("NotificationOnlineFormID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A list of NotificationCarbonCopyStaffs
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNotificationCarbonCopyStaffs <- function(searchConditionsList = NULL, NotificationCarbonCopyStaffID = F, NotificationID = F, StaffID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "NotificationCarbonCopyStaff", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NotificationCarbonCopyStaff
	#'
	#' This function returns a dataframe or json object of a NotificationCarbonCopyStaff
	#' @param NotificationCarbonCopyStaffID The ID of the NotificationCarbonCopyStaff to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NotificationCarbonCopyStaff. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NotificationCarbonCopyStaff.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NotificationCarbonCopyStaff') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A dataframe or of NotificationCarbonCopyStaff
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNotificationCarbonCopyStaff <- function(NotificationCarbonCopyStaffID, NotificationID = F, StaffID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NotificationCarbonCopyStaffID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "NotificationCarbonCopyStaff", objectId = NotificationCarbonCopyStaffID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NotificationCarbonCopyStaff
	#'
	#' This function deletes a NotificationCarbonCopyStaff
	#' @param NotificationCarbonCopyStaffID The ID of the NotificationCarbonCopyStaff to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The NotificationCarbonCopyStaffID of the deleted NotificationCarbonCopyStaff.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNotificationCarbonCopyStaff <- function(NotificationCarbonCopyStaffID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "NotificationCarbonCopyStaff", objectId = NotificationCarbonCopyStaffID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NotificationCarbonCopyStaff
	#'
	#' This function creates a NotificationCarbonCopyStaff
	#' @param fieldNames The field values to give the created NotificationCarbonCopyStaff. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A newly created NotificationCarbonCopyStaff
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNotificationCarbonCopyStaff <- function(NotificationID = NULL, StaffID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "NotificationCarbonCopyStaff", body = list(DataObject = body), searchFields = append("NotificationCarbonCopyStaffID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NotificationCarbonCopyStaff
	#'
	#' This function modifies a NotificationCarbonCopyStaff
	#' @param fieldNames The field values to give the modified NotificationCarbonCopyStaff. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The modified NotificationCarbonCopyStaff
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNotificationCarbonCopyStaff <- function(NotificationCarbonCopyStaffID, NotificationID = NULL, StaffID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "NotificationCarbonCopyStaff", objectId = NotificationCarbonCopyStaffID, body = list(DataObject = body), searchFields = append("NotificationCarbonCopyStaffID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A list of NotificationActions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNotificationActions <- function(searchConditionsList = NULL, NotificationActionID = F, NotificationID = F, ActionID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "NotificationAction", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NotificationAction
	#'
	#' This function returns a dataframe or json object of a NotificationAction
	#' @param NotificationActionID The ID of the NotificationAction to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NotificationAction. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NotificationAction.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NotificationAction') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A dataframe or of NotificationAction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNotificationAction <- function(NotificationActionID, NotificationID = F, ActionID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NotificationActionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "NotificationAction", objectId = NotificationActionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NotificationAction
	#'
	#' This function deletes a NotificationAction
	#' @param NotificationActionID The ID of the NotificationAction to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The NotificationActionID of the deleted NotificationAction.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNotificationAction <- function(NotificationActionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "NotificationAction", objectId = NotificationActionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NotificationAction
	#'
	#' This function creates a NotificationAction
	#' @param fieldNames The field values to give the created NotificationAction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A newly created NotificationAction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNotificationAction <- function(NotificationID = NULL, ActionID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "NotificationAction", body = list(DataObject = body), searchFields = append("NotificationActionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NotificationAction
	#'
	#' This function modifies a NotificationAction
	#' @param fieldNames The field values to give the modified NotificationAction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The modified NotificationAction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNotificationAction <- function(NotificationActionID, NotificationID = NULL, ActionID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "NotificationAction", objectId = NotificationActionID, body = list(DataObject = body), searchFields = append("NotificationActionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A list of NotificationAttendanceTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNotificationAttendanceTypes <- function(searchConditionsList = NULL, NotificationAttendanceTypeID = F, NotificationID = F, AttendanceTypeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "NotificationAttendanceType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NotificationAttendanceType
	#'
	#' This function returns a dataframe or json object of a NotificationAttendanceType
	#' @param NotificationAttendanceTypeID The ID of the NotificationAttendanceType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NotificationAttendanceType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NotificationAttendanceType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NotificationAttendanceType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A dataframe or of NotificationAttendanceType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNotificationAttendanceType <- function(NotificationAttendanceTypeID, NotificationID = F, AttendanceTypeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NotificationAttendanceTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "NotificationAttendanceType", objectId = NotificationAttendanceTypeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NotificationAttendanceType
	#'
	#' This function deletes a NotificationAttendanceType
	#' @param NotificationAttendanceTypeID The ID of the NotificationAttendanceType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The NotificationAttendanceTypeID of the deleted NotificationAttendanceType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNotificationAttendanceType <- function(NotificationAttendanceTypeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "NotificationAttendanceType", objectId = NotificationAttendanceTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NotificationAttendanceType
	#'
	#' This function creates a NotificationAttendanceType
	#' @param fieldNames The field values to give the created NotificationAttendanceType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A newly created NotificationAttendanceType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNotificationAttendanceType <- function(NotificationID = NULL, AttendanceTypeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "NotificationAttendanceType", body = list(DataObject = body), searchFields = append("NotificationAttendanceTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NotificationAttendanceType
	#'
	#' This function modifies a NotificationAttendanceType
	#' @param fieldNames The field values to give the modified NotificationAttendanceType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The modified NotificationAttendanceType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNotificationAttendanceType <- function(NotificationAttendanceTypeID, NotificationID = NULL, AttendanceTypeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "NotificationAttendanceType", objectId = NotificationAttendanceTypeID, body = list(DataObject = body), searchFields = append("NotificationAttendanceTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A list of NotificationGradeBuckets
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNotificationGradeBuckets <- function(searchConditionsList = NULL, NotificationGradeBucketID = F, NotificationID = F, GradeBucketID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "NotificationGradeBucket", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NotificationGradeBucket
	#'
	#' This function returns a dataframe or json object of a NotificationGradeBucket
	#' @param NotificationGradeBucketID The ID of the NotificationGradeBucket to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NotificationGradeBucket. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NotificationGradeBucket.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NotificationGradeBucket') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A dataframe or of NotificationGradeBucket
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNotificationGradeBucket <- function(NotificationGradeBucketID, NotificationID = F, GradeBucketID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NotificationGradeBucketID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "NotificationGradeBucket", objectId = NotificationGradeBucketID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NotificationGradeBucket
	#'
	#' This function deletes a NotificationGradeBucket
	#' @param NotificationGradeBucketID The ID of the NotificationGradeBucket to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The NotificationGradeBucketID of the deleted NotificationGradeBucket.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNotificationGradeBucket <- function(NotificationGradeBucketID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "NotificationGradeBucket", objectId = NotificationGradeBucketID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NotificationGradeBucket
	#'
	#' This function creates a NotificationGradeBucket
	#' @param fieldNames The field values to give the created NotificationGradeBucket. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A newly created NotificationGradeBucket
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNotificationGradeBucket <- function(NotificationID = NULL, GradeBucketID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "NotificationGradeBucket", body = list(DataObject = body), searchFields = append("NotificationGradeBucketID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NotificationGradeBucket
	#'
	#' This function modifies a NotificationGradeBucket
	#' @param fieldNames The field values to give the modified NotificationGradeBucket. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The modified NotificationGradeBucket
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNotificationGradeBucket <- function(NotificationGradeBucketID, NotificationID = NULL, GradeBucketID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "NotificationGradeBucket", objectId = NotificationGradeBucketID, body = list(DataObject = body), searchFields = append("NotificationGradeBucketID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A list of NotificationGradeMarks
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNotificationGradeMarks <- function(searchConditionsList = NULL, NotificationGradeMarkID = F, NotificationID = F, GradeMarkID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "NotificationGradeMark", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NotificationGradeMark
	#'
	#' This function returns a dataframe or json object of a NotificationGradeMark
	#' @param NotificationGradeMarkID The ID of the NotificationGradeMark to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NotificationGradeMark. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NotificationGradeMark.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NotificationGradeMark') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A dataframe or of NotificationGradeMark
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNotificationGradeMark <- function(NotificationGradeMarkID, NotificationID = F, GradeMarkID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NotificationGradeMarkID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "NotificationGradeMark", objectId = NotificationGradeMarkID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NotificationGradeMark
	#'
	#' This function deletes a NotificationGradeMark
	#' @param NotificationGradeMarkID The ID of the NotificationGradeMark to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The NotificationGradeMarkID of the deleted NotificationGradeMark.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNotificationGradeMark <- function(NotificationGradeMarkID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "NotificationGradeMark", objectId = NotificationGradeMarkID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NotificationGradeMark
	#'
	#' This function creates a NotificationGradeMark
	#' @param fieldNames The field values to give the created NotificationGradeMark. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A newly created NotificationGradeMark
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNotificationGradeMark <- function(NotificationID = NULL, GradeMarkID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "NotificationGradeMark", body = list(DataObject = body), searchFields = append("NotificationGradeMarkID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NotificationGradeMark
	#'
	#' This function modifies a NotificationGradeMark
	#' @param fieldNames The field values to give the modified NotificationGradeMark. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The modified NotificationGradeMark
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNotificationGradeMark <- function(NotificationGradeMarkID, NotificationID = NULL, GradeMarkID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "NotificationGradeMark", objectId = NotificationGradeMarkID, body = list(DataObject = body), searchFields = append("NotificationGradeMarkID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A list of NotificationOffenses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNotificationOffenses <- function(searchConditionsList = NULL, NotificationOffenseID = F, NotificationID = F, OffenseID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "NotificationOffense", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NotificationOffense
	#'
	#' This function returns a dataframe or json object of a NotificationOffense
	#' @param NotificationOffenseID The ID of the NotificationOffense to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NotificationOffense. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NotificationOffense.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NotificationOffense') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A dataframe or of NotificationOffense
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNotificationOffense <- function(NotificationOffenseID, NotificationID = F, OffenseID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NotificationOffenseID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "NotificationOffense", objectId = NotificationOffenseID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NotificationOffense
	#'
	#' This function deletes a NotificationOffense
	#' @param NotificationOffenseID The ID of the NotificationOffense to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The NotificationOffenseID of the deleted NotificationOffense.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNotificationOffense <- function(NotificationOffenseID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "NotificationOffense", objectId = NotificationOffenseID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NotificationOffense
	#'
	#' This function creates a NotificationOffense
	#' @param fieldNames The field values to give the created NotificationOffense. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A newly created NotificationOffense
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNotificationOffense <- function(NotificationID = NULL, OffenseID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "NotificationOffense", body = list(DataObject = body), searchFields = append("NotificationOffenseID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NotificationOffense
	#'
	#' This function modifies a NotificationOffense
	#' @param fieldNames The field values to give the modified NotificationOffense. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The modified NotificationOffense
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNotificationOffense <- function(NotificationOffenseID, NotificationID = NULL, OffenseID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "NotificationOffense", objectId = NotificationOffenseID, body = list(DataObject = body), searchFields = append("NotificationOffenseID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A list of TempMessages
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempMessages <- function(searchConditionsList = NULL, TempMessageID = F, RecipientName = F, StudentName = F, Relationship = F, SectionInfo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "TempMessage", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempMessage
	#'
	#' This function returns a dataframe or json object of a TempMessage
	#' @param TempMessageID The ID of the TempMessage to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempMessage. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempMessage.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempMessage') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A dataframe or of TempMessage
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempMessage <- function(TempMessageID, RecipientName = F, StudentName = F, Relationship = F, SectionInfo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempMessageID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "TempMessage", objectId = TempMessageID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempMessage
	#'
	#' This function deletes a TempMessage
	#' @param TempMessageID The ID of the TempMessage to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The TempMessageID of the deleted TempMessage.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempMessage <- function(TempMessageID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "TempMessage", objectId = TempMessageID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempMessage
	#'
	#' This function creates a TempMessage
	#' @param fieldNames The field values to give the created TempMessage. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A newly created TempMessage
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempMessage <- function(RecipientName = NULL, StudentName = NULL, Relationship = NULL, SectionInfo = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "TempMessage", body = list(DataObject = body), searchFields = append("TempMessageID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempMessage
	#'
	#' This function modifies a TempMessage
	#' @param fieldNames The field values to give the modified TempMessage. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The modified TempMessage
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempMessage <- function(TempMessageID, RecipientName = NULL, StudentName = NULL, Relationship = NULL, SectionInfo = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "TempMessage", objectId = TempMessageID, body = list(DataObject = body), searchFields = append("TempMessageID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A list of Notifications
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNotifications <- function(searchConditionsList = NULL, NotificationID = F, Subject = F, Message = F, Type = F, PriorityType = F, XMLFilter = F, EntityID = F, SchoolYearID = F, ScheduledTaskID = F, AttachmentCount = F, FoodServiceBalanceLow = F, FoodServiceBalanceHigh = F, FeeManagementBalanceLow = F, FeeManagementBalanceHigh = F, GradingPeriodEndDaysBefore = F, GradingPeriodEndDaysAfter = F, ScheduleIsAvailableDaysBefore = F, AttendanceCountMethod = F, AttendanceCategoryForCount = F, AttendanceCountLow = F, AttendanceCountHigh = F, SendNotificationForDay = F, SendNotificationForPriorDayCount = F, SendOnlyIfGuardianNotNotified = F, IncludeAutoGeneratedMessage = F, SendToDisciplineOfficer = F, DayType = F, NumberOfDays = F, LastEntryType = F, ConsiderAllStaffMeets = F, UnrecordedAttendancePeriodType = F, UnrecordedAttendanceMinutes = F, ToWhom = F, SubjectCleaned = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, WarningMessage = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "Notification", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Notification
	#'
	#' This function returns a dataframe or json object of a Notification
	#' @param NotificationID The ID of the Notification to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Notification. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Notification.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Notification') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A dataframe or of Notification
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNotification <- function(NotificationID, Subject = F, Message = F, Type = F, PriorityType = F, XMLFilter = F, EntityID = F, SchoolYearID = F, ScheduledTaskID = F, AttachmentCount = F, FoodServiceBalanceLow = F, FoodServiceBalanceHigh = F, FeeManagementBalanceLow = F, FeeManagementBalanceHigh = F, GradingPeriodEndDaysBefore = F, GradingPeriodEndDaysAfter = F, ScheduleIsAvailableDaysBefore = F, AttendanceCountMethod = F, AttendanceCategoryForCount = F, AttendanceCountLow = F, AttendanceCountHigh = F, SendNotificationForDay = F, SendNotificationForPriorDayCount = F, SendOnlyIfGuardianNotNotified = F, IncludeAutoGeneratedMessage = F, SendToDisciplineOfficer = F, DayType = F, NumberOfDays = F, LastEntryType = F, ConsiderAllStaffMeets = F, UnrecordedAttendancePeriodType = F, UnrecordedAttendanceMinutes = F, ToWhom = F, SubjectCleaned = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, WarningMessage = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NotificationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "Notification", objectId = NotificationID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Notification
	#'
	#' This function deletes a Notification
	#' @param NotificationID The ID of the Notification to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The NotificationID of the deleted Notification.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNotification <- function(NotificationID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "Notification", objectId = NotificationID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Notification
	#'
	#' This function creates a Notification
	#' @param fieldNames The field values to give the created Notification. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A newly created Notification
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNotification <- function(Subject = NULL, Message = NULL, Type = NULL, PriorityType = NULL, XMLFilter = NULL, EntityID = NULL, SchoolYearID = NULL, ScheduledTaskID = NULL, FoodServiceBalanceLow = NULL, FoodServiceBalanceHigh = NULL, FeeManagementBalanceLow = NULL, FeeManagementBalanceHigh = NULL, GradingPeriodEndDaysBefore = NULL, GradingPeriodEndDaysAfter = NULL, ScheduleIsAvailableDaysBefore = NULL, AttendanceCountMethod = NULL, AttendanceCategoryForCount = NULL, AttendanceCountLow = NULL, AttendanceCountHigh = NULL, SendNotificationForDay = NULL, SendNotificationForPriorDayCount = NULL, SendOnlyIfGuardianNotNotified = NULL, IncludeAutoGeneratedMessage = NULL, SendToDisciplineOfficer = NULL, DayType = NULL, NumberOfDays = NULL, LastEntryType = NULL, ConsiderAllStaffMeets = NULL, UnrecordedAttendancePeriodType = NULL, UnrecordedAttendanceMinutes = NULL, ToWhom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "Notification", body = list(DataObject = body), searchFields = append("NotificationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Notification
	#'
	#' This function modifies a Notification
	#' @param fieldNames The field values to give the modified Notification. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The modified Notification
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNotification <- function(NotificationID, Subject = NULL, Message = NULL, Type = NULL, PriorityType = NULL, XMLFilter = NULL, EntityID = NULL, SchoolYearID = NULL, ScheduledTaskID = NULL, FoodServiceBalanceLow = NULL, FoodServiceBalanceHigh = NULL, FeeManagementBalanceLow = NULL, FeeManagementBalanceHigh = NULL, GradingPeriodEndDaysBefore = NULL, GradingPeriodEndDaysAfter = NULL, ScheduleIsAvailableDaysBefore = NULL, AttendanceCountMethod = NULL, AttendanceCategoryForCount = NULL, AttendanceCountLow = NULL, AttendanceCountHigh = NULL, SendNotificationForDay = NULL, SendNotificationForPriorDayCount = NULL, SendOnlyIfGuardianNotNotified = NULL, IncludeAutoGeneratedMessage = NULL, SendToDisciplineOfficer = NULL, DayType = NULL, NumberOfDays = NULL, LastEntryType = NULL, ConsiderAllStaffMeets = NULL, UnrecordedAttendancePeriodType = NULL, UnrecordedAttendanceMinutes = NULL, ToWhom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "Notification", objectId = NotificationID, body = list(DataObject = body), searchFields = append("NotificationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A list of UserMessageSettings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listUserMessageSettings <- function(searchConditionsList = NULL, UserMessageSettingID = F, UserIDOwner = F, CopyMessagesToEmail = F, CopyDisciplineMessagesToEmail = F, CopyFeeManagementMessagesToEmail = F, CopyFoodServiceMessagesToEmail = F, CopyGradingMessagesToEmail = F, CopySchedulingMessagesToEmail = F, CopyAttendanceMessagesToEmail = F, CopyOnlineFormMessagesToEmail = F, EnableCompletedGradeChangeNotification = F, EnableCompletedGradeChangeNotificationEmail = F, CopyGradebookMessagesToEmail = F, GradebookHighThreshold = F, GradebookLowThreshold = F, GradebookLowAssignmentThreshold = F, AssignmentScoreLowNotification = F, AssignmentScoreLowNotificationEmail = F, GradebookHighAssignmentThreshold = F, AssignmentScoreHighNotification = F, AssignmentScoreHighNotificationEmail = F, CurrentGradeScoreLowNotification = F, CurrentGradeScoreLowNotificationEmail = F, CurrentGradeScoreHighNotification = F, CurrentGradeScoreHighNotificationEmail = F, MissingAssignmentNotification = F, MissingAssignmentNotificationEmail = F, EnableGradebookLastEntryNotificationEmail = F, EnableGradebookGradeChangeRequestDeniedEmail = F, EnableGradebookGradeChangeRequestNotificationEmail = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EnableOnlineAssignmentAvailableNotificationEmail = F, EnableOnlineAssingmentScoresAvailableNotificationEmail = F, CopyEnrollmentMessagesToEmail = F, CopyGraduationRequirementsMessagesToEmail = F, EnableCompletedCareerPlanChangeNotification = F, EnableCompletedCareerPlanChangeNotificationEmail = F, OnlySendAssignmentScoreLowNotificationsOncePerAssignment = F, OnlySendAssignmentScoreHighNotificationsOncePerAssignment = F, OnlySendCurrentGradeScoreLowNotificationsOnce = F, OnlySendCurrentGradeScoreHighNotificationsOnce = F, OnlySendMissingAssignmentNotificationForCurrentGradingPeriod = F, EnableStudentScheduleChangeNotification = F, EnableStudentScheduleChangeNotificationEmail = F, OnlySendMissingAssignmentNotificationsOncePerAssignment = F, CopyFamilyAccessMessagesToEmail = F, CopyMTSSMessagesToEmail = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "UserMessageSetting", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an UserMessageSetting
	#'
	#' This function returns a dataframe or json object of an UserMessageSetting
	#' @param UserMessageSettingID The ID of the UserMessageSetting to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given UserMessageSetting. Defaults to FALSE for all return fields which, for convenience, returns all fields for the UserMessageSetting.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('UserMessageSetting') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A dataframe or of UserMessageSetting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getUserMessageSetting <- function(UserMessageSettingID, UserIDOwner = F, CopyMessagesToEmail = F, CopyDisciplineMessagesToEmail = F, CopyFeeManagementMessagesToEmail = F, CopyFoodServiceMessagesToEmail = F, CopyGradingMessagesToEmail = F, CopySchedulingMessagesToEmail = F, CopyAttendanceMessagesToEmail = F, CopyOnlineFormMessagesToEmail = F, EnableCompletedGradeChangeNotification = F, EnableCompletedGradeChangeNotificationEmail = F, CopyGradebookMessagesToEmail = F, GradebookHighThreshold = F, GradebookLowThreshold = F, GradebookLowAssignmentThreshold = F, AssignmentScoreLowNotification = F, AssignmentScoreLowNotificationEmail = F, GradebookHighAssignmentThreshold = F, AssignmentScoreHighNotification = F, AssignmentScoreHighNotificationEmail = F, CurrentGradeScoreLowNotification = F, CurrentGradeScoreLowNotificationEmail = F, CurrentGradeScoreHighNotification = F, CurrentGradeScoreHighNotificationEmail = F, MissingAssignmentNotification = F, MissingAssignmentNotificationEmail = F, EnableGradebookLastEntryNotificationEmail = F, EnableGradebookGradeChangeRequestDeniedEmail = F, EnableGradebookGradeChangeRequestNotificationEmail = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EnableOnlineAssignmentAvailableNotificationEmail = F, EnableOnlineAssingmentScoresAvailableNotificationEmail = F, CopyEnrollmentMessagesToEmail = F, CopyGraduationRequirementsMessagesToEmail = F, EnableCompletedCareerPlanChangeNotification = F, EnableCompletedCareerPlanChangeNotificationEmail = F, OnlySendAssignmentScoreLowNotificationsOncePerAssignment = F, OnlySendAssignmentScoreHighNotificationsOncePerAssignment = F, OnlySendCurrentGradeScoreLowNotificationsOnce = F, OnlySendCurrentGradeScoreHighNotificationsOnce = F, OnlySendMissingAssignmentNotificationForCurrentGradingPeriod = F, EnableStudentScheduleChangeNotification = F, EnableStudentScheduleChangeNotificationEmail = F, OnlySendMissingAssignmentNotificationsOncePerAssignment = F, CopyFamilyAccessMessagesToEmail = F, CopyMTSSMessagesToEmail = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "UserMessageSettingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "UserMessageSetting", objectId = UserMessageSettingID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an UserMessageSetting
	#'
	#' This function deletes an UserMessageSetting
	#' @param UserMessageSettingID The ID of the UserMessageSetting to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The UserMessageSettingID of the deleted UserMessageSetting.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteUserMessageSetting <- function(UserMessageSettingID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "UserMessageSetting", objectId = UserMessageSettingID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an UserMessageSetting
	#'
	#' This function creates an UserMessageSetting
	#' @param fieldNames The field values to give the created UserMessageSetting. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A newly created UserMessageSetting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createUserMessageSetting <- function(UserIDOwner = NULL, CopyMessagesToEmail = NULL, CopyDisciplineMessagesToEmail = NULL, CopyFeeManagementMessagesToEmail = NULL, CopyFoodServiceMessagesToEmail = NULL, CopyGradingMessagesToEmail = NULL, CopySchedulingMessagesToEmail = NULL, CopyAttendanceMessagesToEmail = NULL, CopyOnlineFormMessagesToEmail = NULL, EnableCompletedGradeChangeNotification = NULL, EnableCompletedGradeChangeNotificationEmail = NULL, CopyGradebookMessagesToEmail = NULL, GradebookHighThreshold = NULL, GradebookLowThreshold = NULL, GradebookLowAssignmentThreshold = NULL, AssignmentScoreLowNotification = NULL, AssignmentScoreLowNotificationEmail = NULL, GradebookHighAssignmentThreshold = NULL, AssignmentScoreHighNotification = NULL, AssignmentScoreHighNotificationEmail = NULL, CurrentGradeScoreLowNotification = NULL, CurrentGradeScoreLowNotificationEmail = NULL, CurrentGradeScoreHighNotification = NULL, CurrentGradeScoreHighNotificationEmail = NULL, MissingAssignmentNotification = NULL, MissingAssignmentNotificationEmail = NULL, EnableGradebookLastEntryNotificationEmail = NULL, EnableGradebookGradeChangeRequestDeniedEmail = NULL, EnableGradebookGradeChangeRequestNotificationEmail = NULL, EnableOnlineAssignmentAvailableNotificationEmail = NULL, EnableOnlineAssingmentScoresAvailableNotificationEmail = NULL, CopyEnrollmentMessagesToEmail = NULL, CopyGraduationRequirementsMessagesToEmail = NULL, EnableCompletedCareerPlanChangeNotification = NULL, EnableCompletedCareerPlanChangeNotificationEmail = NULL, OnlySendAssignmentScoreLowNotificationsOncePerAssignment = NULL, OnlySendAssignmentScoreHighNotificationsOncePerAssignment = NULL, OnlySendCurrentGradeScoreLowNotificationsOnce = NULL, OnlySendCurrentGradeScoreHighNotificationsOnce = NULL, OnlySendMissingAssignmentNotificationForCurrentGradingPeriod = NULL, EnableStudentScheduleChangeNotification = NULL, EnableStudentScheduleChangeNotificationEmail = NULL, OnlySendMissingAssignmentNotificationsOncePerAssignment = NULL, CopyFamilyAccessMessagesToEmail = NULL, CopyMTSSMessagesToEmail = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "UserMessageSetting", body = list(DataObject = body), searchFields = append("UserMessageSettingID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an UserMessageSetting
	#'
	#' This function modifies an UserMessageSetting
	#' @param fieldNames The field values to give the modified UserMessageSetting. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The modified UserMessageSetting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyUserMessageSetting <- function(UserMessageSettingID, UserIDOwner = NULL, CopyMessagesToEmail = NULL, CopyDisciplineMessagesToEmail = NULL, CopyFeeManagementMessagesToEmail = NULL, CopyFoodServiceMessagesToEmail = NULL, CopyGradingMessagesToEmail = NULL, CopySchedulingMessagesToEmail = NULL, CopyAttendanceMessagesToEmail = NULL, CopyOnlineFormMessagesToEmail = NULL, EnableCompletedGradeChangeNotification = NULL, EnableCompletedGradeChangeNotificationEmail = NULL, CopyGradebookMessagesToEmail = NULL, GradebookHighThreshold = NULL, GradebookLowThreshold = NULL, GradebookLowAssignmentThreshold = NULL, AssignmentScoreLowNotification = NULL, AssignmentScoreLowNotificationEmail = NULL, GradebookHighAssignmentThreshold = NULL, AssignmentScoreHighNotification = NULL, AssignmentScoreHighNotificationEmail = NULL, CurrentGradeScoreLowNotification = NULL, CurrentGradeScoreLowNotificationEmail = NULL, CurrentGradeScoreHighNotification = NULL, CurrentGradeScoreHighNotificationEmail = NULL, MissingAssignmentNotification = NULL, MissingAssignmentNotificationEmail = NULL, EnableGradebookLastEntryNotificationEmail = NULL, EnableGradebookGradeChangeRequestDeniedEmail = NULL, EnableGradebookGradeChangeRequestNotificationEmail = NULL, EnableOnlineAssignmentAvailableNotificationEmail = NULL, EnableOnlineAssingmentScoresAvailableNotificationEmail = NULL, CopyEnrollmentMessagesToEmail = NULL, CopyGraduationRequirementsMessagesToEmail = NULL, EnableCompletedCareerPlanChangeNotification = NULL, EnableCompletedCareerPlanChangeNotificationEmail = NULL, OnlySendAssignmentScoreLowNotificationsOncePerAssignment = NULL, OnlySendAssignmentScoreHighNotificationsOncePerAssignment = NULL, OnlySendCurrentGradeScoreLowNotificationsOnce = NULL, OnlySendCurrentGradeScoreHighNotificationsOnce = NULL, OnlySendMissingAssignmentNotificationForCurrentGradingPeriod = NULL, EnableStudentScheduleChangeNotification = NULL, EnableStudentScheduleChangeNotificationEmail = NULL, OnlySendMissingAssignmentNotificationsOncePerAssignment = NULL, CopyFamilyAccessMessagesToEmail = NULL, CopyMTSSMessagesToEmail = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "UserMessageSetting", objectId = UserMessageSettingID, body = list(DataObject = body), searchFields = append("UserMessageSettingID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A list of Messages
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listMessages <- function(searchConditionsList = NULL, MessageID = F, MessageMasterID = F, UserIDRecipient = F, PostTime = F, IsHidden = F, IsRead = F, MessageIDCopiedFrom = F, Subject = F, Content = F, PriorityType = F, Type = F, NoSourceIDRequired = F, ObjectIDCreatedFor = F, SourceIDCreatedFor = F, FromInformation = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ObjectPrimaryKey = F, EmailRecipientTypeCode = F, EmailsSent = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "Message", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Message
	#'
	#' This function returns a dataframe or json object of a Message
	#' @param MessageID The ID of the Message to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Message. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Message.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Message') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A dataframe or of Message
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getMessage <- function(MessageID, MessageMasterID = F, UserIDRecipient = F, PostTime = F, IsHidden = F, IsRead = F, MessageIDCopiedFrom = F, Subject = F, Content = F, PriorityType = F, Type = F, NoSourceIDRequired = F, ObjectIDCreatedFor = F, SourceIDCreatedFor = F, FromInformation = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ObjectPrimaryKey = F, EmailRecipientTypeCode = F, EmailsSent = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "MessageID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "Message", objectId = MessageID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Message
	#'
	#' This function deletes a Message
	#' @param MessageID The ID of the Message to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The MessageID of the deleted Message.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteMessage <- function(MessageID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "Message", objectId = MessageID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Message
	#'
	#' This function creates a Message
	#' @param fieldNames The field values to give the created Message. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A newly created Message
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createMessage <- function(MessageMasterID = NULL, UserIDRecipient = NULL, IsHidden = NULL, IsRead = NULL, MessageIDCopiedFrom = NULL, Subject = NULL, Content = NULL, PriorityType = NULL, Type = NULL, ObjectIDCreatedFor = NULL, SourceIDCreatedFor = NULL, FromInformation = NULL, ObjectPrimaryKey = NULL, EmailRecipientTypeCode = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "Message", body = list(DataObject = body), searchFields = append("MessageID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Message
	#'
	#' This function modifies a Message
	#' @param fieldNames The field values to give the modified Message. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The modified Message
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyMessage <- function(MessageID, MessageMasterID = NULL, UserIDRecipient = NULL, IsHidden = NULL, IsRead = NULL, MessageIDCopiedFrom = NULL, Subject = NULL, Content = NULL, PriorityType = NULL, Type = NULL, ObjectIDCreatedFor = NULL, SourceIDCreatedFor = NULL, FromInformation = NULL, ObjectPrimaryKey = NULL, EmailRecipientTypeCode = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "Message", objectId = MessageID, body = list(DataObject = body), searchFields = append("MessageID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A list of MessageMasters
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listMessageMasters <- function(searchConditionsList = NULL, MessageMasterID = F, Subject = F, Content = F, PostedTime = F, Type = F, PriorityType = F, XMLFilter = F, SubjectCleaned = F, IsDraft = F, ObjectIDCreatedFor = F, SourceIDCreatedFor = F, EntityID = F, SchoolYearID = F, ToWhom = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IncludeRestrictedGuardians = F, IsRetracted = F, Status = F, LargestMessagePrimaryKey = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "MessageMaster", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a MessageMaster
	#'
	#' This function returns a dataframe or json object of a MessageMaster
	#' @param MessageMasterID The ID of the MessageMaster to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given MessageMaster. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MessageMaster.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MessageMaster') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A dataframe or of MessageMaster
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getMessageMaster <- function(MessageMasterID, Subject = F, Content = F, PostedTime = F, Type = F, PriorityType = F, XMLFilter = F, SubjectCleaned = F, IsDraft = F, ObjectIDCreatedFor = F, SourceIDCreatedFor = F, EntityID = F, SchoolYearID = F, ToWhom = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IncludeRestrictedGuardians = F, IsRetracted = F, Status = F, LargestMessagePrimaryKey = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "MessageMasterID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "MessageMaster", objectId = MessageMasterID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a MessageMaster
	#'
	#' This function deletes a MessageMaster
	#' @param MessageMasterID The ID of the MessageMaster to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The MessageMasterID of the deleted MessageMaster.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteMessageMaster <- function(MessageMasterID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "MessageMaster", objectId = MessageMasterID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a MessageMaster
	#'
	#' This function creates a MessageMaster
	#' @param fieldNames The field values to give the created MessageMaster. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A newly created MessageMaster
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createMessageMaster <- function(Subject = NULL, Content = NULL, PostedTime = NULL, Type = NULL, PriorityType = NULL, XMLFilter = NULL, IsDraft = NULL, ObjectIDCreatedFor = NULL, SourceIDCreatedFor = NULL, EntityID = NULL, SchoolYearID = NULL, ToWhom = NULL, IncludeRestrictedGuardians = NULL, Status = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "MessageMaster", body = list(DataObject = body), searchFields = append("MessageMasterID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a MessageMaster
	#'
	#' This function modifies a MessageMaster
	#' @param fieldNames The field values to give the modified MessageMaster. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The modified MessageMaster
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyMessageMaster <- function(MessageMasterID, Subject = NULL, Content = NULL, PostedTime = NULL, Type = NULL, PriorityType = NULL, XMLFilter = NULL, IsDraft = NULL, ObjectIDCreatedFor = NULL, SourceIDCreatedFor = NULL, EntityID = NULL, SchoolYearID = NULL, ToWhom = NULL, IncludeRestrictedGuardians = NULL, Status = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "MessageMaster", objectId = MessageMasterID, body = list(DataObject = body), searchFields = append("MessageMasterID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A list of NotificationWithdrawalCodes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNotificationWithdrawalCodes <- function(searchConditionsList = NULL, NotificationWithdrawalCodeID = F, NotificationID = F, WithdrawalCodeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "NotificationWithdrawalCode", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NotificationWithdrawalCode
	#'
	#' This function returns a dataframe or json object of a NotificationWithdrawalCode
	#' @param NotificationWithdrawalCodeID The ID of the NotificationWithdrawalCode to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NotificationWithdrawalCode. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NotificationWithdrawalCode.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NotificationWithdrawalCode') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A dataframe or of NotificationWithdrawalCode
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNotificationWithdrawalCode <- function(NotificationWithdrawalCodeID, NotificationID = F, WithdrawalCodeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NotificationWithdrawalCodeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "NotificationWithdrawalCode", objectId = NotificationWithdrawalCodeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NotificationWithdrawalCode
	#'
	#' This function deletes a NotificationWithdrawalCode
	#' @param NotificationWithdrawalCodeID The ID of the NotificationWithdrawalCode to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The NotificationWithdrawalCodeID of the deleted NotificationWithdrawalCode.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNotificationWithdrawalCode <- function(NotificationWithdrawalCodeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "NotificationWithdrawalCode", objectId = NotificationWithdrawalCodeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NotificationWithdrawalCode
	#'
	#' This function creates a NotificationWithdrawalCode
	#' @param fieldNames The field values to give the created NotificationWithdrawalCode. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A newly created NotificationWithdrawalCode
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNotificationWithdrawalCode <- function(NotificationID = NULL, WithdrawalCodeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "NotificationWithdrawalCode", body = list(DataObject = body), searchFields = append("NotificationWithdrawalCodeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NotificationWithdrawalCode
	#'
	#' This function modifies a NotificationWithdrawalCode
	#' @param fieldNames The field values to give the modified NotificationWithdrawalCode. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The modified NotificationWithdrawalCode
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNotificationWithdrawalCode <- function(NotificationWithdrawalCodeID, NotificationID = NULL, WithdrawalCodeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "NotificationWithdrawalCode", objectId = NotificationWithdrawalCodeID, body = list(DataObject = body), searchFields = append("NotificationWithdrawalCodeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A list of QueuedCompletedCareerPlanChangeNotifications
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listQueuedCompletedCareerPlanChangeNotifications <- function(searchConditionsList = NULL, QueuedCompletedCareerPlanChangeNotificationID = F, NotificationID = F, StudentCareerPlanID = F, StudentID = F, EntityID = F, SchoolYearID = F, CurriculumID = F, StudentSubAreaIDPrevious = F, StudentSubAreaIDCurrent = F, StudentCourseRequestIDPrevious = F, StudentCourseRequestIDCurrent = F, CareerPlanGradeLevelIDPrevious = F, CareerPlanGradeLevelIDCurrent = F, IsStudentPermittedToChangeGradeLevelPrevious = F, IsStudentPermittedToChangeGradeLevelCurrent = F, IsStudentPermittedToDeletePrevious = F, IsStudentPermittedToDeleteCurrent = F, CreditsPrevious = F, CreditsCurrent = F, IsNewRecord = F, IsDeletedRecord = F, IsSent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "QueuedCompletedCareerPlanChangeNotification", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a QueuedCompletedCareerPlanChangeNotification
	#'
	#' This function returns a dataframe or json object of a QueuedCompletedCareerPlanChangeNotification
	#' @param QueuedCompletedCareerPlanChangeNotificationID The ID of the QueuedCompletedCareerPlanChangeNotification to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given QueuedCompletedCareerPlanChangeNotification. Defaults to FALSE for all return fields which, for convenience, returns all fields for the QueuedCompletedCareerPlanChangeNotification.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('QueuedCompletedCareerPlanChangeNotification') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A dataframe or of QueuedCompletedCareerPlanChangeNotification
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getQueuedCompletedCareerPlanChangeNotification <- function(QueuedCompletedCareerPlanChangeNotificationID, NotificationID = F, StudentCareerPlanID = F, StudentID = F, EntityID = F, SchoolYearID = F, CurriculumID = F, StudentSubAreaIDPrevious = F, StudentSubAreaIDCurrent = F, StudentCourseRequestIDPrevious = F, StudentCourseRequestIDCurrent = F, CareerPlanGradeLevelIDPrevious = F, CareerPlanGradeLevelIDCurrent = F, IsStudentPermittedToChangeGradeLevelPrevious = F, IsStudentPermittedToChangeGradeLevelCurrent = F, IsStudentPermittedToDeletePrevious = F, IsStudentPermittedToDeleteCurrent = F, CreditsPrevious = F, CreditsCurrent = F, IsNewRecord = F, IsDeletedRecord = F, IsSent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "QueuedCompletedCareerPlanChangeNotificationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "QueuedCompletedCareerPlanChangeNotification", objectId = QueuedCompletedCareerPlanChangeNotificationID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a QueuedCompletedCareerPlanChangeNotification
	#'
	#' This function deletes a QueuedCompletedCareerPlanChangeNotification
	#' @param QueuedCompletedCareerPlanChangeNotificationID The ID of the QueuedCompletedCareerPlanChangeNotification to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The QueuedCompletedCareerPlanChangeNotificationID of the deleted QueuedCompletedCareerPlanChangeNotification.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteQueuedCompletedCareerPlanChangeNotification <- function(QueuedCompletedCareerPlanChangeNotificationID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "QueuedCompletedCareerPlanChangeNotification", objectId = QueuedCompletedCareerPlanChangeNotificationID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a QueuedCompletedCareerPlanChangeNotification
	#'
	#' This function creates a QueuedCompletedCareerPlanChangeNotification
	#' @param fieldNames The field values to give the created QueuedCompletedCareerPlanChangeNotification. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A newly created QueuedCompletedCareerPlanChangeNotification
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createQueuedCompletedCareerPlanChangeNotification <- function(NotificationID = NULL, StudentCareerPlanID = NULL, StudentID = NULL, EntityID = NULL, SchoolYearID = NULL, CurriculumID = NULL, StudentSubAreaIDPrevious = NULL, StudentSubAreaIDCurrent = NULL, StudentCourseRequestIDPrevious = NULL, StudentCourseRequestIDCurrent = NULL, CareerPlanGradeLevelIDPrevious = NULL, CareerPlanGradeLevelIDCurrent = NULL, IsStudentPermittedToChangeGradeLevelPrevious = NULL, IsStudentPermittedToChangeGradeLevelCurrent = NULL, IsStudentPermittedToDeletePrevious = NULL, IsStudentPermittedToDeleteCurrent = NULL, CreditsPrevious = NULL, CreditsCurrent = NULL, IsNewRecord = NULL, IsDeletedRecord = NULL, IsSent = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "QueuedCompletedCareerPlanChangeNotification", body = list(DataObject = body), searchFields = append("QueuedCompletedCareerPlanChangeNotificationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a QueuedCompletedCareerPlanChangeNotification
	#'
	#' This function modifies a QueuedCompletedCareerPlanChangeNotification
	#' @param fieldNames The field values to give the modified QueuedCompletedCareerPlanChangeNotification. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The modified QueuedCompletedCareerPlanChangeNotification
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyQueuedCompletedCareerPlanChangeNotification <- function(QueuedCompletedCareerPlanChangeNotificationID, NotificationID = NULL, StudentCareerPlanID = NULL, StudentID = NULL, EntityID = NULL, SchoolYearID = NULL, CurriculumID = NULL, StudentSubAreaIDPrevious = NULL, StudentSubAreaIDCurrent = NULL, StudentCourseRequestIDPrevious = NULL, StudentCourseRequestIDCurrent = NULL, CareerPlanGradeLevelIDPrevious = NULL, CareerPlanGradeLevelIDCurrent = NULL, IsStudentPermittedToChangeGradeLevelPrevious = NULL, IsStudentPermittedToChangeGradeLevelCurrent = NULL, IsStudentPermittedToDeletePrevious = NULL, IsStudentPermittedToDeleteCurrent = NULL, CreditsPrevious = NULL, CreditsCurrent = NULL, IsNewRecord = NULL, IsDeletedRecord = NULL, IsSent = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "QueuedCompletedCareerPlanChangeNotification", objectId = QueuedCompletedCareerPlanChangeNotificationID, body = list(DataObject = body), searchFields = append("QueuedCompletedCareerPlanChangeNotificationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A list of MessageCenterConfigDistrictYears
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listMessageCenterConfigDistrictYears <- function(searchConditionsList = NULL, ConfigDistrictYearID = F, DistrictID = F, SchoolYearID = F, NumberOfDaysAfterWithdrawalToAllowMessages = F, ConfigDistrictYearIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "ConfigDistrictYear", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a MessageCenterConfigDistrictYear
	#'
	#' This function returns a dataframe or json object of a MessageCenterConfigDistrictYear
	#' @param MessageCenterConfigDistrictYearID The ID of the MessageCenterConfigDistrictYear to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given MessageCenterConfigDistrictYear. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MessageCenterConfigDistrictYear.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MessageCenterConfigDistrictYear') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A dataframe or of MessageCenterConfigDistrictYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getMessageCenterConfigDistrictYear <- function(MessageCenterConfigDistrictYearID, ConfigDistrictYearID = F, DistrictID = F, SchoolYearID = F, NumberOfDaysAfterWithdrawalToAllowMessages = F, ConfigDistrictYearIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "MessageCenterConfigDistrictYearID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "ConfigDistrictYear", objectId = MessageCenterConfigDistrictYearID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a MessageCenterConfigDistrictYear
	#'
	#' This function deletes a MessageCenterConfigDistrictYear
	#' @param MessageCenterConfigDistrictYearID The ID of the MessageCenterConfigDistrictYear to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The MessageCenterConfigDistrictYearID of the deleted MessageCenterConfigDistrictYear.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteMessageCenterConfigDistrictYear <- function(MessageCenterConfigDistrictYearID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "ConfigDistrictYear", objectId = MessageCenterConfigDistrictYearID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a MessageCenterConfigDistrictYear
	#'
	#' This function creates a MessageCenterConfigDistrictYear
	#' @param fieldNames The field values to give the created MessageCenterConfigDistrictYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A newly created MessageCenterConfigDistrictYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createMessageCenterConfigDistrictYear <- function(DistrictID = NULL, SchoolYearID = NULL, NumberOfDaysAfterWithdrawalToAllowMessages = NULL, ConfigDistrictYearIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "ConfigDistrictYear", body = list(DataObject = body), searchFields = append("ConfigDistrictYearID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a MessageCenterConfigDistrictYear
	#'
	#' This function modifies a MessageCenterConfigDistrictYear
	#' @param fieldNames The field values to give the modified MessageCenterConfigDistrictYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The modified MessageCenterConfigDistrictYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyMessageCenterConfigDistrictYear <- function(ConfigDistrictYearID, DistrictID = NULL, SchoolYearID = NULL, NumberOfDaysAfterWithdrawalToAllowMessages = NULL, ConfigDistrictYearIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "ConfigDistrictYear", objectId = ConfigDistrictYearID, body = list(DataObject = body), searchFields = append("ConfigDistrictYearID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A list of MessageCenterConfigDistrictYearWithdrawalCodes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listMessageCenterConfigDistrictYearWithdrawalCodes <- function(searchConditionsList = NULL, ConfigDistrictYearWithdrawalCodeID = F, ConfigDistrictYearID = F, WithdrawalCodeID = F, ConfigDistrictYearWithdrawalCodeIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "ConfigDistrictYearWithdrawalCode", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a MessageCenterConfigDistrictYearWithdrawalCode
	#'
	#' This function returns a dataframe or json object of a MessageCenterConfigDistrictYearWithdrawalCode
	#' @param MessageCenterConfigDistrictYearWithdrawalCodeID The ID of the MessageCenterConfigDistrictYearWithdrawalCode to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given MessageCenterConfigDistrictYearWithdrawalCode. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MessageCenterConfigDistrictYearWithdrawalCode.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MessageCenterConfigDistrictYearWithdrawalCode') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A dataframe or of MessageCenterConfigDistrictYearWithdrawalCode
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getMessageCenterConfigDistrictYearWithdrawalCode <- function(MessageCenterConfigDistrictYearWithdrawalCodeID, ConfigDistrictYearWithdrawalCodeID = F, ConfigDistrictYearID = F, WithdrawalCodeID = F, ConfigDistrictYearWithdrawalCodeIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "MessageCenterConfigDistrictYearWithdrawalCodeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "ConfigDistrictYearWithdrawalCode", objectId = MessageCenterConfigDistrictYearWithdrawalCodeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a MessageCenterConfigDistrictYearWithdrawalCode
	#'
	#' This function deletes a MessageCenterConfigDistrictYearWithdrawalCode
	#' @param MessageCenterConfigDistrictYearWithdrawalCodeID The ID of the MessageCenterConfigDistrictYearWithdrawalCode to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The MessageCenterConfigDistrictYearWithdrawalCodeID of the deleted MessageCenterConfigDistrictYearWithdrawalCode.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteMessageCenterConfigDistrictYearWithdrawalCode <- function(MessageCenterConfigDistrictYearWithdrawalCodeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "ConfigDistrictYearWithdrawalCode", objectId = MessageCenterConfigDistrictYearWithdrawalCodeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a MessageCenterConfigDistrictYearWithdrawalCode
	#'
	#' This function creates a MessageCenterConfigDistrictYearWithdrawalCode
	#' @param fieldNames The field values to give the created MessageCenterConfigDistrictYearWithdrawalCode. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A newly created MessageCenterConfigDistrictYearWithdrawalCode
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createMessageCenterConfigDistrictYearWithdrawalCode <- function(ConfigDistrictYearID = NULL, WithdrawalCodeID = NULL, ConfigDistrictYearWithdrawalCodeIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "ConfigDistrictYearWithdrawalCode", body = list(DataObject = body), searchFields = append("ConfigDistrictYearWithdrawalCodeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a MessageCenterConfigDistrictYearWithdrawalCode
	#'
	#' This function modifies a MessageCenterConfigDistrictYearWithdrawalCode
	#' @param fieldNames The field values to give the modified MessageCenterConfigDistrictYearWithdrawalCode. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The modified MessageCenterConfigDistrictYearWithdrawalCode
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyMessageCenterConfigDistrictYearWithdrawalCode <- function(ConfigDistrictYearWithdrawalCodeID, ConfigDistrictYearID = NULL, WithdrawalCodeID = NULL, ConfigDistrictYearWithdrawalCodeIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "ConfigDistrictYearWithdrawalCode", objectId = ConfigDistrictYearWithdrawalCodeID, body = list(DataObject = body), searchFields = append("ConfigDistrictYearWithdrawalCodeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
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
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A list of SystemEmailTypeConfigs
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSystemEmailTypeConfigs <- function(searchConditionsList = NULL, SystemEmailTypeConfigID = F, EmailTypeID = F, Rank = F, EmailRecipientType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MessageCenter", objectName = "SystemEmailTypeConfig", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SystemEmailTypeConfig
	#'
	#' This function returns a dataframe or json object of a SystemEmailTypeConfig
	#' @param SystemEmailTypeConfigID The ID of the SystemEmailTypeConfig to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SystemEmailTypeConfig. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SystemEmailTypeConfig.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SystemEmailTypeConfig') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A dataframe or of SystemEmailTypeConfig
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSystemEmailTypeConfig <- function(SystemEmailTypeConfigID, EmailTypeID = F, Rank = F, EmailRecipientType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SystemEmailTypeConfigID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MessageCenter", objectName = "SystemEmailTypeConfig", objectId = SystemEmailTypeConfigID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SystemEmailTypeConfig
	#'
	#' This function deletes a SystemEmailTypeConfig
	#' @param SystemEmailTypeConfigID The ID of the SystemEmailTypeConfig to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The SystemEmailTypeConfigID of the deleted SystemEmailTypeConfig.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSystemEmailTypeConfig <- function(SystemEmailTypeConfigID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MessageCenter", objectName = "SystemEmailTypeConfig", objectId = SystemEmailTypeConfigID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SystemEmailTypeConfig
	#'
	#' This function creates a SystemEmailTypeConfig
	#' @param fieldNames The field values to give the created SystemEmailTypeConfig. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return A newly created SystemEmailTypeConfig
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSystemEmailTypeConfig <- function(EmailTypeID = NULL, Rank = NULL, EmailRecipientType = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MessageCenter", objectName = "SystemEmailTypeConfig", body = list(DataObject = body), searchFields = append("SystemEmailTypeConfigID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SystemEmailTypeConfig
	#'
	#' This function modifies a SystemEmailTypeConfig
	#' @param fieldNames The field values to give the modified SystemEmailTypeConfig. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Message Center
	#' @return The modified SystemEmailTypeConfig
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySystemEmailTypeConfig <- function(SystemEmailTypeConfigID, EmailTypeID = NULL, Rank = NULL, EmailRecipientType = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MessageCenter", objectName = "SystemEmailTypeConfig", objectId = SystemEmailTypeConfigID, body = list(DataObject = body), searchFields = append("SystemEmailTypeConfigID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}
