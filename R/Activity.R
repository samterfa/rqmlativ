
	#' List TempStudentActivities
	#'
	#' This function returns a dataframe or json object of TempStudentActivities
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempStudentActivity') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @concept Activity
	#' @return A list of TempStudentActivities
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStudentActivities <- function(searchConditionsList = NULL, TempStudentActivityID = F, ActivityID = F, Code = F, Description = F, StudentID = F, StudentName = F, StartDate = F, EndDate = F, Age = F, GradeLevel = F, ParticipationStartDate = F, ParticipationEndDate = F, FullNameLFM = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StudentNumber = F, GradYear = F, GenderCodeAndGender = F, EntityCode = F, IsActive = F, ErrorNumber = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Activity", objectName = "TempStudentActivity", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten)
	}

	#' List TempActivityRecords
	#'
	#' This function returns a dataframe or json object of TempActivityRecords
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempActivityRecord') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @concept Activity
	#' @return A list of TempActivityRecords
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempActivityRecords <- function(searchConditionsList = NULL, TempActivityRecordID = F, ActivityID = F, Code = F, Description = F, NewStartDate = F, NewEndDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Activity", objectName = "TempActivityRecord", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten)
	}

	#' List TempStudentActivityTransactionRecords
	#'
	#' This function returns a dataframe or json object of TempStudentActivityTransactionRecords
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempStudentActivityTransactionRecord') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @concept Activity
	#' @return A list of TempStudentActivityTransactionRecords
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStudentActivityTransactionRecords <- function(searchConditionsList = NULL, TempStudentActivityTransactionRecordID = F, ActivityID = F, StudentActivityTransactionID = F, StudentFullNameLFM = F, NewStartDate = F, NewEndDate = F, IsValidTransaction = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Activity", objectName = "TempStudentActivityTransactionRecord", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten)
	}

	#' List ActivityEvents
	#'
	#' This function returns a dataframe or json object of ActivityEvents
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('ActivityEvent') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @concept Activity
	#' @return A list of ActivityEvents
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listActivityEvents <- function(searchConditionsList = NULL, ActivityEventID = F, ActivityID = F, EventTypeID = F, Summary = F, Description = F, CodeSummary = F, Cost = F, Status = F, NameIDContact = F, StartTime = F, EndTime = F, LocationID = F, VisibleTo = F, ContactType = F, DisplayOnDistrictCalendar = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Date = F, DescriptionSummary = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Activity", objectName = "ActivityEvent", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten)
	}

	#' List EventTypes
	#'
	#' This function returns a dataframe or json object of EventTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('EventType') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @concept Activity
	#' @return A list of EventTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEventTypes <- function(searchConditionsList = NULL, EventTypeID = F, Code = F, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Activity", objectName = "EventType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten)
	}

	#' List StudentActivityTransactions
	#'
	#' This function returns a dataframe or json object of StudentActivityTransactions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentActivityTransaction') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @concept Activity
	#' @return A list of StudentActivityTransactions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentActivityTransactions <- function(searchConditionsList = NULL, StudentActivityTransactionID = F, StudentActivityID = F, ParticipationStartDate = F, ParticipationEndDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Activity", objectName = "StudentActivityTransaction", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten)
	}

	#' List Activities
	#'
	#' This function returns a dataframe or json object of Activities
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('Activity') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @concept Activity
	#' @return A list of Activities
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listActivities <- function(searchConditionsList = NULL, ActivityID = F, Code = F, Description = F, EntityID = F, SchoolYearID = F, ActivityTypeID = F, StartDate = F, EndDate = F, CodeDescription = F, ActivityIDClonedFrom = F, UseForAthleticEligibilityReporting = F, ActivityLeaderNames = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HideInFamilyAccess = F, SingleSex = F, ActivityIDClonedTo = F, StudentAwardID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Activity", objectName = "Activity", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten)
	}

	#' List ActivityTypes
	#'
	#' This function returns a dataframe or json object of ActivityTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('ActivityType') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @concept Activity
	#' @return A list of ActivityTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listActivityTypes <- function(searchConditionsList = NULL, ActivityTypeID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EdFiCohortTypeDescriptorID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Activity", objectName = "ActivityType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten)
	}

	#' List ActivityPersonnels
	#'
	#' This function returns a dataframe or json object of ActivityPersonnels
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('ActivityPersonnel') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @concept Activity
	#' @return A list of ActivityPersonnels
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listActivityPersonnels <- function(searchConditionsList = NULL, ActivityPersonnelID = F, ActivityID = F, NameID = F, IsLeader = F, ActivityPersonnelIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Activity", objectName = "ActivityPersonnel", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten)
	}

	#' List StudentActivities
	#'
	#' This function returns a dataframe or json object of StudentActivities
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('StudentActivity') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @concept Activity
	#' @return A list of StudentActivities
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentActivities <- function(searchConditionsList = NULL, StudentActivityID = F, StudentID = F, ActivityID = F, CourseID = F, IsActive = F, StudentActivityIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasParticipation = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Activity", objectName = "StudentActivity", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten)
	}

	#' List TempStudentActivityErrors
	#'
	#' This function returns a dataframe or json object of TempStudentActivityErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempStudentActivityError') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @concept Activity
	#' @return A list of TempStudentActivityErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStudentActivityErrors <- function(searchConditionsList = NULL, TempStudentActivityErrorID = F, TempStudentActivityID = F, ErrorNumber = F, ErrorText = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Activity", objectName = "TempStudentActivityError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten)
	}

	#' List ActivityConfigDistricts
	#'
	#' This function returns a dataframe or json object of ActivityConfigDistricts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('ActivityConfigDistrict') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @concept Activity
	#' @return A list of ActivityConfigDistricts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listActivityConfigDistricts <- function(searchConditionsList = NULL, ConfigDistrictID = F, DistrictID = F, StructureDimensionID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Activity", objectName = "ConfigDistrict", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten)
	}

	#' List ConfigDistrictStructureDimensionPositions
	#'
	#' This function returns a dataframe or json object of ConfigDistrictStructureDimensionPositions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('ConfigDistrictStructureDimensionPosition') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @concept Activity
	#' @return A list of ConfigDistrictStructureDimensionPositions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listConfigDistrictStructureDimensionPositions <- function(searchConditionsList = NULL, ConfigDistrictStructureDimensionPositionID = F, ConfigDistrictID = F, Position = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Activity", objectName = "ConfigDistrictStructureDimensionPosition", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten)
	}

	#' List ActivityAccountingGroups
	#'
	#' This function returns a dataframe or json object of ActivityAccountingGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('ActivityAccountingGroup') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @concept Activity
	#' @return A list of ActivityAccountingGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listActivityAccountingGroups <- function(searchConditionsList = NULL, ActivityAccountingGroupID = F, EntityID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AccountEntityOverlay = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Activity", objectName = "ActivityAccountingGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten)
	}

	#' List ActivityAccountingGroupClearances
	#'
	#' This function returns a dataframe or json object of ActivityAccountingGroupClearances
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('ActivityAccountingGroupClearance') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @concept Activity
	#' @return A list of ActivityAccountingGroupClearances
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listActivityAccountingGroupClearances <- function(searchConditionsList = NULL, ActivityAccountingGroupClearanceID = F, ActivityAccountingGroupID = F, GroupIDSecurity = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Activity", objectName = "ActivityAccountingGroupClearance", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten)
	}

	#' List TempActivityAccountingGroupMassUpdates
	#'
	#' This function returns a dataframe or json object of TempActivityAccountingGroupMassUpdates
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempActivityAccountingGroupMassUpdate') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @concept Activity
	#' @return A list of TempActivityAccountingGroupMassUpdates
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempActivityAccountingGroupMassUpdates <- function(searchConditionsList = NULL, TempActivityAccountingGroupMassUpdateID = F, UpdateType = F, ActivityAccountingGroupIDOld = F, ActivityAccountingGroupOld = F, ActivityAccountingGroupIDNew = F, ActivityAccountingGroupNew = F, UpdateObjectID = F, UpdateObjectDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Activity", objectName = "TempActivityAccountingGroupMassUpdate", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten)
	}

	#' List TempClearances
	#'
	#' This function returns a dataframe or json object of TempClearances
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempClearance') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @concept Activity
	#' @return A list of TempClearances
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempClearances <- function(searchConditionsList = NULL, TempClearanceID = F, GroupIDSecurity = F, GroupID = F, GroupDescription = F, EntityName = F, ClearanceType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Activity", objectName = "TempClearance", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten)
	}

	#' List TempEntityBalanceErrors
	#'
	#' This function returns a dataframe or json object of TempEntityBalanceErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempEntityBalanceError') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @concept Activity
	#' @return A list of TempEntityBalanceErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempEntityBalanceErrors <- function(searchConditionsList = NULL, TempEntityBalanceErrorID = F, FundDimensionID = F, FundDimensionCodeDescription = F, ActivityAccountingGroupID = F, EntityCodeName = F, Error = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Activity", objectName = "TempEntityBalanceError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten)
	}

	#' List TempActivityAccountErrors
	#'
	#' This function returns a dataframe or json object of TempActivityAccountErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempActivityAccountError') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @concept Activity
	#' @return A list of TempActivityAccountErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempActivityAccountErrors <- function(searchConditionsList = NULL, TempActivityAccountErrorID = F, AccountID = F, DisplayAccount = F, FundDimensionID = F, FundDimensionCodeDescription = F, Error = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Activity", objectName = "TempActivityAccountError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten)
	}

	#' List TempActivityAccounts
	#'
	#' This function returns a dataframe or json object of TempActivityAccounts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempActivityAccount') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @concept Activity
	#' @return A list of TempActivityAccounts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempActivityAccounts <- function(searchConditionsList = NULL, TempActivityAccountID = F, AccountID = F, DisplayAccount = F, FundDimensionID = F, FundDimensionCodeDescription = F, ActivityAccountingGroupIDOld = F, ActivityAccountingGroupIDNew = F, EntityCodeNameOld = F, EntityCodeNameNew = F, ExpenseSummaryIDOld = F, ExpenseSummaryOld = F, ExpenseSummaryIDNew = F, ExpenseSummaryNew = F, RevenueSummaryIDOld = F, RevenueSummaryOld = F, RevenueSummaryIDNew = F, RevenueSummaryNew = F, EncumbranceOffsetIDOld = F, EncumbranceOffsetOld = F, EncumbranceOffsetIDNew = F, EncumbranceOffsetNew = F, ReservedForEncumbranceIDOld = F, ReservedForEncumbranceOld = F, ReservedForEncumbranceIDNew = F, ReservedForEncumbranceNew = F, HasError = F, ErrorCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Activity", objectName = "TempActivityAccount", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten)
	}
