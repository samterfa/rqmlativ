
	#' List ActionAttendanceTypes
	#'
	#' This function returns a dataframe or json object of ActionAttendanceTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ActionAttendanceTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ActionAttendanceTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ActionAttendanceType') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of ActionAttendanceTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listActionAttendanceTypes <- function(searchConditionsList = NULL, ActionAttendanceTypeID = F, ActionID = F, AttendanceTypeID = F, EntityGroupKey = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, ActionAttendanceTypeIDClonedFrom = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "ActionAttendanceType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ActionAttendanceType
	#'
	#' This function returns a dataframe or json object of an ActionAttendanceType
	#' @param ActionAttendanceTypeID The ID of the ActionAttendanceType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ActionAttendanceType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ActionAttendanceType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ActionAttendanceType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of ActionAttendanceType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getActionAttendanceType <- function(ActionAttendanceTypeID, ActionID = F, AttendanceTypeID = F, EntityGroupKey = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, ActionAttendanceTypeIDClonedFrom = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ActionAttendanceTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "ActionAttendanceType", objectId = ActionAttendanceTypeID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ActionAttendanceType
	#'
	#' This function deletes an ActionAttendanceType
	#' @param ActionAttendanceTypeID The ID of the ActionAttendanceType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The ActionAttendanceTypeID of the deleted ActionAttendanceType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteActionAttendanceType <- function(ActionAttendanceTypeID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "ActionAttendanceType", objectId = ActionAttendanceTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ActionAttendanceType
	#'
	#' This function creates an ActionAttendanceType
	#' @param fieldNames The field values to give the created ActionAttendanceType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created ActionAttendanceType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createActionAttendanceType <- function(ActionID = NULL, AttendanceTypeID = NULL, EntityGroupKey = NULL, ActionAttendanceTypeIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "ActionAttendanceType", body = list(DataObject = body), searchFields = append("ActionAttendanceTypeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ActionAttendanceType
	#'
	#' This function modifies an ActionAttendanceType
	#' @param fieldNames The field values to give the modified ActionAttendanceType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified ActionAttendanceType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyActionAttendanceType <- function(ActionAttendanceTypeID, ActionID = NULL, AttendanceTypeID = NULL, EntityGroupKey = NULL, ActionAttendanceTypeIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "ActionAttendanceType", objectId = ActionAttendanceTypeID, body = list(DataObject = body), searchFields = append("ActionAttendanceTypeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List IncidentOffenseNameActionDetailPeriods
	#'
	#' This function returns a dataframe or json object of IncidentOffenseNameActionDetailPeriods
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given IncidentOffenseNameActionDetailPeriods. Defaults to FALSE for all return fields which, for convenience, returns all fields for the IncidentOffenseNameActionDetailPeriods.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('IncidentOffenseNameActionDetailPeriod') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of IncidentOffenseNameActionDetailPeriods
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listIncidentOffenseNameActionDetailPeriods <- function(searchConditionsList = NULL, IncidentOffenseNameActionDetailPeriodID = F, IncidentOffenseNameActionDetailID = F, AttendancePeriodID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "IncidentOffenseNameActionDetailPeriod", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an IncidentOffenseNameActionDetailPeriod
	#'
	#' This function returns a dataframe or json object of an IncidentOffenseNameActionDetailPeriod
	#' @param IncidentOffenseNameActionDetailPeriodID The ID of the IncidentOffenseNameActionDetailPeriod to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given IncidentOffenseNameActionDetailPeriod. Defaults to FALSE for all return fields which, for convenience, returns all fields for the IncidentOffenseNameActionDetailPeriod.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('IncidentOffenseNameActionDetailPeriod') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of IncidentOffenseNameActionDetailPeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getIncidentOffenseNameActionDetailPeriod <- function(IncidentOffenseNameActionDetailPeriodID, IncidentOffenseNameActionDetailID = F, AttendancePeriodID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "IncidentOffenseNameActionDetailPeriodID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "IncidentOffenseNameActionDetailPeriod", objectId = IncidentOffenseNameActionDetailPeriodID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an IncidentOffenseNameActionDetailPeriod
	#'
	#' This function deletes an IncidentOffenseNameActionDetailPeriod
	#' @param IncidentOffenseNameActionDetailPeriodID The ID of the IncidentOffenseNameActionDetailPeriod to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The IncidentOffenseNameActionDetailPeriodID of the deleted IncidentOffenseNameActionDetailPeriod.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteIncidentOffenseNameActionDetailPeriod <- function(IncidentOffenseNameActionDetailPeriodID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "IncidentOffenseNameActionDetailPeriod", objectId = IncidentOffenseNameActionDetailPeriodID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an IncidentOffenseNameActionDetailPeriod
	#'
	#' This function creates an IncidentOffenseNameActionDetailPeriod
	#' @param fieldNames The field values to give the created IncidentOffenseNameActionDetailPeriod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created IncidentOffenseNameActionDetailPeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createIncidentOffenseNameActionDetailPeriod <- function(IncidentOffenseNameActionDetailID = NULL, AttendancePeriodID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "IncidentOffenseNameActionDetailPeriod", body = list(DataObject = body), searchFields = append("IncidentOffenseNameActionDetailPeriodID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an IncidentOffenseNameActionDetailPeriod
	#'
	#' This function modifies an IncidentOffenseNameActionDetailPeriod
	#' @param fieldNames The field values to give the modified IncidentOffenseNameActionDetailPeriod. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified IncidentOffenseNameActionDetailPeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyIncidentOffenseNameActionDetailPeriod <- function(IncidentOffenseNameActionDetailPeriodID, IncidentOffenseNameActionDetailID = NULL, AttendancePeriodID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "IncidentOffenseNameActionDetailPeriod", objectId = IncidentOffenseNameActionDetailPeriodID, body = list(DataObject = body), searchFields = append("IncidentOffenseNameActionDetailPeriodID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DisciplineConfigEntityGroupYears
	#'
	#' This function returns a dataframe or json object of DisciplineConfigEntityGroupYears
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DisciplineConfigEntityGroupYears. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DisciplineConfigEntityGroupYears.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DisciplineConfigEntityGroupYear') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of DisciplineConfigEntityGroupYears
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDisciplineConfigEntityGroupYears <- function(searchConditionsList = NULL, ConfigEntityGroupYearID = F, EntityID = F, EntityGroupKey = F, SchoolYearID = F, UseAlternateActionDetails = F, DetentionsOnMon = F, DetentionsOnTue = F, DetentionsOnWed = F, DetentionsOnThu = F, DetentionsOnFri = F, DetentionsOnSat = F, DetentionsOnSun = F, InSchoolSuspensionsOnMon = F, InSchoolSuspensionsOnTue = F, InSchoolSuspensionsOnWed = F, InSchoolSuspensionsOnThu = F, InSchoolSuspensionsOnFri = F, InSchoolSuspensionsOnSat = F, InSchoolSuspensionsOnSun = F, OutOfSchoolSuspensionsOnMon = F, OutOfSchoolSuspensionsOnTue = F, OutOfSchoolSuspensionsOnWed = F, OutOfSchoolSuspensionsOnThu = F, OutOfSchoolSuspensionsOnFri = F, OutOfSchoolSuspensionsOnSat = F, OutOfSchoolSuspensionsOnSun = F, ExpulsionsOnMon = F, ExpulsionsOnTue = F, ExpulsionsOnWed = F, ExpulsionsOnThu = F, ExpulsionsOnFri = F, ExpulsionsOnSat = F, ExpulsionsOnSun = F, DefaultActionStatusCode = F, TardyKioskDisciplineSlipTitle = F, IncludeStudentNameAndOrNumberOnLetter = F, IncludeGradeLevelOnLetter = F, IncludeSchoolOrCampusOnLetter = F, IncludeParentNameAndOrPhoneNumberOnLetter = F, IncludeDisciplinaryActionAndDetailsOnLetter = F, IncludeSignatureLineForParentOnLetter = F, IncludeSignatureLineForStudentOnLetter = F, IncludeSignatureLineForOfficeOnLetter = F, ActionStatusDefaultValue = F, ConfigEntityGroupYearIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, IncludeOffenseDescriptionOnLetter = F, IncludeIncidentDateAndTimeOnLetter = F, IncludeIncidentDescriptionOnLetter = F, IncludeReferredByOnLetter = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "ConfigEntityGroupYear", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DisciplineConfigEntityGroupYear
	#'
	#' This function returns a dataframe or json object of a DisciplineConfigEntityGroupYear
	#' @param DisciplineConfigEntityGroupYearID The ID of the DisciplineConfigEntityGroupYear to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DisciplineConfigEntityGroupYear. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DisciplineConfigEntityGroupYear.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DisciplineConfigEntityGroupYear') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of DisciplineConfigEntityGroupYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDisciplineConfigEntityGroupYear <- function(DisciplineConfigEntityGroupYearID, ConfigEntityGroupYearID = F, EntityID = F, EntityGroupKey = F, SchoolYearID = F, UseAlternateActionDetails = F, DetentionsOnMon = F, DetentionsOnTue = F, DetentionsOnWed = F, DetentionsOnThu = F, DetentionsOnFri = F, DetentionsOnSat = F, DetentionsOnSun = F, InSchoolSuspensionsOnMon = F, InSchoolSuspensionsOnTue = F, InSchoolSuspensionsOnWed = F, InSchoolSuspensionsOnThu = F, InSchoolSuspensionsOnFri = F, InSchoolSuspensionsOnSat = F, InSchoolSuspensionsOnSun = F, OutOfSchoolSuspensionsOnMon = F, OutOfSchoolSuspensionsOnTue = F, OutOfSchoolSuspensionsOnWed = F, OutOfSchoolSuspensionsOnThu = F, OutOfSchoolSuspensionsOnFri = F, OutOfSchoolSuspensionsOnSat = F, OutOfSchoolSuspensionsOnSun = F, ExpulsionsOnMon = F, ExpulsionsOnTue = F, ExpulsionsOnWed = F, ExpulsionsOnThu = F, ExpulsionsOnFri = F, ExpulsionsOnSat = F, ExpulsionsOnSun = F, DefaultActionStatusCode = F, TardyKioskDisciplineSlipTitle = F, IncludeStudentNameAndOrNumberOnLetter = F, IncludeGradeLevelOnLetter = F, IncludeSchoolOrCampusOnLetter = F, IncludeParentNameAndOrPhoneNumberOnLetter = F, IncludeDisciplinaryActionAndDetailsOnLetter = F, IncludeSignatureLineForParentOnLetter = F, IncludeSignatureLineForStudentOnLetter = F, IncludeSignatureLineForOfficeOnLetter = F, ActionStatusDefaultValue = F, ConfigEntityGroupYearIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, IncludeOffenseDescriptionOnLetter = F, IncludeIncidentDateAndTimeOnLetter = F, IncludeIncidentDescriptionOnLetter = F, IncludeReferredByOnLetter = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DisciplineConfigEntityGroupYearID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "ConfigEntityGroupYear", objectId = DisciplineConfigEntityGroupYearID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DisciplineConfigEntityGroupYear
	#'
	#' This function deletes a DisciplineConfigEntityGroupYear
	#' @param DisciplineConfigEntityGroupYearID The ID of the DisciplineConfigEntityGroupYear to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The DisciplineConfigEntityGroupYearID of the deleted DisciplineConfigEntityGroupYear.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDisciplineConfigEntityGroupYear <- function(DisciplineConfigEntityGroupYearID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "ConfigEntityGroupYear", objectId = DisciplineConfigEntityGroupYearID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DisciplineConfigEntityGroupYear
	#'
	#' This function creates a DisciplineConfigEntityGroupYear
	#' @param fieldNames The field values to give the created DisciplineConfigEntityGroupYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created DisciplineConfigEntityGroupYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDisciplineConfigEntityGroupYear <- function(EntityID = NULL, EntityGroupKey = NULL, SchoolYearID = NULL, UseAlternateActionDetails = NULL, DetentionsOnMon = NULL, DetentionsOnTue = NULL, DetentionsOnWed = NULL, DetentionsOnThu = NULL, DetentionsOnFri = NULL, DetentionsOnSat = NULL, DetentionsOnSun = NULL, InSchoolSuspensionsOnMon = NULL, InSchoolSuspensionsOnTue = NULL, InSchoolSuspensionsOnWed = NULL, InSchoolSuspensionsOnThu = NULL, InSchoolSuspensionsOnFri = NULL, InSchoolSuspensionsOnSat = NULL, InSchoolSuspensionsOnSun = NULL, OutOfSchoolSuspensionsOnMon = NULL, OutOfSchoolSuspensionsOnTue = NULL, OutOfSchoolSuspensionsOnWed = NULL, OutOfSchoolSuspensionsOnThu = NULL, OutOfSchoolSuspensionsOnFri = NULL, OutOfSchoolSuspensionsOnSat = NULL, OutOfSchoolSuspensionsOnSun = NULL, ExpulsionsOnMon = NULL, ExpulsionsOnTue = NULL, ExpulsionsOnWed = NULL, ExpulsionsOnThu = NULL, ExpulsionsOnFri = NULL, ExpulsionsOnSat = NULL, ExpulsionsOnSun = NULL, DefaultActionStatusCode = NULL, TardyKioskDisciplineSlipTitle = NULL, IncludeStudentNameAndOrNumberOnLetter = NULL, IncludeGradeLevelOnLetter = NULL, IncludeSchoolOrCampusOnLetter = NULL, IncludeParentNameAndOrPhoneNumberOnLetter = NULL, IncludeDisciplinaryActionAndDetailsOnLetter = NULL, IncludeSignatureLineForParentOnLetter = NULL, IncludeSignatureLineForStudentOnLetter = NULL, IncludeSignatureLineForOfficeOnLetter = NULL, ActionStatusDefaultValue = NULL, ConfigEntityGroupYearIDClonedFrom = NULL, IncludeOffenseDescriptionOnLetter = NULL, IncludeIncidentDateAndTimeOnLetter = NULL, IncludeIncidentDescriptionOnLetter = NULL, IncludeReferredByOnLetter = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "ConfigEntityGroupYear", body = list(DataObject = body), searchFields = append("ConfigEntityGroupYearID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DisciplineConfigEntityGroupYear
	#'
	#' This function modifies a DisciplineConfigEntityGroupYear
	#' @param fieldNames The field values to give the modified DisciplineConfigEntityGroupYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified DisciplineConfigEntityGroupYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDisciplineConfigEntityGroupYear <- function(ConfigEntityGroupYearID, EntityID = NULL, EntityGroupKey = NULL, SchoolYearID = NULL, UseAlternateActionDetails = NULL, DetentionsOnMon = NULL, DetentionsOnTue = NULL, DetentionsOnWed = NULL, DetentionsOnThu = NULL, DetentionsOnFri = NULL, DetentionsOnSat = NULL, DetentionsOnSun = NULL, InSchoolSuspensionsOnMon = NULL, InSchoolSuspensionsOnTue = NULL, InSchoolSuspensionsOnWed = NULL, InSchoolSuspensionsOnThu = NULL, InSchoolSuspensionsOnFri = NULL, InSchoolSuspensionsOnSat = NULL, InSchoolSuspensionsOnSun = NULL, OutOfSchoolSuspensionsOnMon = NULL, OutOfSchoolSuspensionsOnTue = NULL, OutOfSchoolSuspensionsOnWed = NULL, OutOfSchoolSuspensionsOnThu = NULL, OutOfSchoolSuspensionsOnFri = NULL, OutOfSchoolSuspensionsOnSat = NULL, OutOfSchoolSuspensionsOnSun = NULL, ExpulsionsOnMon = NULL, ExpulsionsOnTue = NULL, ExpulsionsOnWed = NULL, ExpulsionsOnThu = NULL, ExpulsionsOnFri = NULL, ExpulsionsOnSat = NULL, ExpulsionsOnSun = NULL, DefaultActionStatusCode = NULL, TardyKioskDisciplineSlipTitle = NULL, IncludeStudentNameAndOrNumberOnLetter = NULL, IncludeGradeLevelOnLetter = NULL, IncludeSchoolOrCampusOnLetter = NULL, IncludeParentNameAndOrPhoneNumberOnLetter = NULL, IncludeDisciplinaryActionAndDetailsOnLetter = NULL, IncludeSignatureLineForParentOnLetter = NULL, IncludeSignatureLineForStudentOnLetter = NULL, IncludeSignatureLineForOfficeOnLetter = NULL, ActionStatusDefaultValue = NULL, ConfigEntityGroupYearIDClonedFrom = NULL, IncludeOffenseDescriptionOnLetter = NULL, IncludeIncidentDateAndTimeOnLetter = NULL, IncludeIncidentDescriptionOnLetter = NULL, IncludeReferredByOnLetter = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "ConfigEntityGroupYear", objectId = ConfigEntityGroupYearID, body = list(DataObject = body), searchFields = append("ConfigEntityGroupYearID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempIncidentOffenseNameActionDetails
	#'
	#' This function returns a dataframe or json object of TempIncidentOffenseNameActionDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentOffenseNameActionDetails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentOffenseNameActionDetails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentOffenseNameActionDetail') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of TempIncidentOffenseNameActionDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempIncidentOffenseNameActionDetails <- function(searchConditionsList = NULL, TempIncidentOffenseNameActionDetailID = F, IncidentOffenseNameActionDetailID = F, FullName = F, InvolvementType = F, OffenseCodeDescription = F, IsPrimaryOffense = F, ActionCodeDescription = F, DurationToServe = F, DurationType = F, CreateAttendance = F, IsAlternate = F, LocationID = F, PartialDayPeriods = F, OldStatus = F, NewStatus = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TempIncidentOffenseNameActionID = F, ScheduledTime = F, DurationServed = F, StaffIDFollowUpOfficer = F, Status = F, IsGuardianNotified = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffenseNameActionDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempIncidentOffenseNameActionDetail
	#'
	#' This function returns a dataframe or json object of a TempIncidentOffenseNameActionDetail
	#' @param TempIncidentOffenseNameActionDetailID The ID of the TempIncidentOffenseNameActionDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentOffenseNameActionDetail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentOffenseNameActionDetail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentOffenseNameActionDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of TempIncidentOffenseNameActionDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempIncidentOffenseNameActionDetail <- function(TempIncidentOffenseNameActionDetailID, IncidentOffenseNameActionDetailID = F, FullName = F, InvolvementType = F, OffenseCodeDescription = F, IsPrimaryOffense = F, ActionCodeDescription = F, DurationToServe = F, DurationType = F, CreateAttendance = F, IsAlternate = F, LocationID = F, PartialDayPeriods = F, OldStatus = F, NewStatus = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TempIncidentOffenseNameActionID = F, ScheduledTime = F, DurationServed = F, StaffIDFollowUpOfficer = F, Status = F, IsGuardianNotified = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempIncidentOffenseNameActionDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameActionDetail", objectId = TempIncidentOffenseNameActionDetailID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempIncidentOffenseNameActionDetail
	#'
	#' This function deletes a TempIncidentOffenseNameActionDetail
	#' @param TempIncidentOffenseNameActionDetailID The ID of the TempIncidentOffenseNameActionDetail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The TempIncidentOffenseNameActionDetailID of the deleted TempIncidentOffenseNameActionDetail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempIncidentOffenseNameActionDetail <- function(TempIncidentOffenseNameActionDetailID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameActionDetail", objectId = TempIncidentOffenseNameActionDetailID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempIncidentOffenseNameActionDetail
	#'
	#' This function creates a TempIncidentOffenseNameActionDetail
	#' @param fieldNames The field values to give the created TempIncidentOffenseNameActionDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created TempIncidentOffenseNameActionDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempIncidentOffenseNameActionDetail <- function(IncidentOffenseNameActionDetailID = NULL, FullName = NULL, InvolvementType = NULL, OffenseCodeDescription = NULL, IsPrimaryOffense = NULL, ActionCodeDescription = NULL, DurationToServe = NULL, DurationType = NULL, CreateAttendance = NULL, IsAlternate = NULL, LocationID = NULL, PartialDayPeriods = NULL, OldStatus = NULL, NewStatus = NULL, TempIncidentOffenseNameActionID = NULL, ScheduledTime = NULL, DurationServed = NULL, StaffIDFollowUpOfficer = NULL, Status = NULL, IsGuardianNotified = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameActionDetail", body = list(DataObject = body), searchFields = append("TempIncidentOffenseNameActionDetailID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempIncidentOffenseNameActionDetail
	#'
	#' This function modifies a TempIncidentOffenseNameActionDetail
	#' @param fieldNames The field values to give the modified TempIncidentOffenseNameActionDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified TempIncidentOffenseNameActionDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempIncidentOffenseNameActionDetail <- function(TempIncidentOffenseNameActionDetailID, IncidentOffenseNameActionDetailID = NULL, FullName = NULL, InvolvementType = NULL, OffenseCodeDescription = NULL, IsPrimaryOffense = NULL, ActionCodeDescription = NULL, DurationToServe = NULL, DurationType = NULL, CreateAttendance = NULL, IsAlternate = NULL, LocationID = NULL, PartialDayPeriods = NULL, OldStatus = NULL, NewStatus = NULL, TempIncidentOffenseNameActionID = NULL, ScheduledTime = NULL, DurationServed = NULL, StaffIDFollowUpOfficer = NULL, Status = NULL, IsGuardianNotified = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameActionDetail", objectId = TempIncidentOffenseNameActionDetailID, body = list(DataObject = body), searchFields = append("TempIncidentOffenseNameActionDetailID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempIncidentOffenseNameActions
	#'
	#' This function returns a dataframe or json object of TempIncidentOffenseNameActions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentOffenseNameActions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentOffenseNameActions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentOffenseNameAction') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of TempIncidentOffenseNameActions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempIncidentOffenseNameActions <- function(searchConditionsList = NULL, FullName = F, InvolvementType = F, OffenseCodeDescription = F, IsPrimaryOffense = F, ActionCodeDescription = F, DurationToServe = F, StaffIDAuthorizedByName = F, PerceivedMotivationCodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Comment = F, StudentNumber = F, TempIncidentOffenseNameActionID = F, EntityID = F, ActionTypeID = F, ActionTypeCode = F, ActionID = F, Status = F, OrderedDate = F, DurationType = F, StaffIDAuthorizedBy = F, StaffIDFollowUpOfficer = F, LocationID = F, BuildingID = F, RoomID = F, TempIncidentOffenseNameID = F, IsGuardianNotified = F, StartTime = F, InternalComment = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffenseNameAction", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempIncidentOffenseNameAction
	#'
	#' This function returns a dataframe or json object of a TempIncidentOffenseNameAction
	#' @param TempIncidentOffenseNameActionID The ID of the TempIncidentOffenseNameAction to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentOffenseNameAction. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentOffenseNameAction.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentOffenseNameAction') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of TempIncidentOffenseNameAction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempIncidentOffenseNameAction <- function(TempIncidentOffenseNameActionID, FullName = F, InvolvementType = F, OffenseCodeDescription = F, IsPrimaryOffense = F, ActionCodeDescription = F, DurationToServe = F, StaffIDAuthorizedByName = F, PerceivedMotivationCodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Comment = F, StudentNumber = F, EntityID = F, ActionTypeID = F, ActionTypeCode = F, ActionID = F, Status = F, OrderedDate = F, DurationType = F, StaffIDAuthorizedBy = F, StaffIDFollowUpOfficer = F, LocationID = F, BuildingID = F, RoomID = F, TempIncidentOffenseNameID = F, IsGuardianNotified = F, StartTime = F, InternalComment = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempIncidentOffenseNameActionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameAction", objectId = TempIncidentOffenseNameActionID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempIncidentOffenseNameAction
	#'
	#' This function deletes a TempIncidentOffenseNameAction
	#' @param TempIncidentOffenseNameActionID The ID of the TempIncidentOffenseNameAction to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The TempIncidentOffenseNameActionID of the deleted TempIncidentOffenseNameAction.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempIncidentOffenseNameAction <- function(TempIncidentOffenseNameActionID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameAction", objectId = TempIncidentOffenseNameActionID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempIncidentOffenseNameAction
	#'
	#' This function creates a TempIncidentOffenseNameAction
	#' @param fieldNames The field values to give the created TempIncidentOffenseNameAction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created TempIncidentOffenseNameAction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempIncidentOffenseNameAction <- function(FullName = NULL, InvolvementType = NULL, OffenseCodeDescription = NULL, IsPrimaryOffense = NULL, ActionCodeDescription = NULL, DurationToServe = NULL, StaffIDAuthorizedByName = NULL, PerceivedMotivationCodeDescription = NULL, Comment = NULL, StudentNumber = NULL, EntityID = NULL, ActionTypeID = NULL, ActionTypeCode = NULL, ActionID = NULL, Status = NULL, OrderedDate = NULL, DurationType = NULL, StaffIDAuthorizedBy = NULL, StaffIDFollowUpOfficer = NULL, LocationID = NULL, BuildingID = NULL, RoomID = NULL, TempIncidentOffenseNameID = NULL, IsGuardianNotified = NULL, StartTime = NULL, InternalComment = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameAction", body = list(DataObject = body), searchFields = append("TempIncidentOffenseNameActionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempIncidentOffenseNameAction
	#'
	#' This function modifies a TempIncidentOffenseNameAction
	#' @param fieldNames The field values to give the modified TempIncidentOffenseNameAction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified TempIncidentOffenseNameAction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempIncidentOffenseNameAction <- function(TempIncidentOffenseNameActionID, FullName = NULL, InvolvementType = NULL, OffenseCodeDescription = NULL, IsPrimaryOffense = NULL, ActionCodeDescription = NULL, DurationToServe = NULL, StaffIDAuthorizedByName = NULL, PerceivedMotivationCodeDescription = NULL, Comment = NULL, StudentNumber = NULL, EntityID = NULL, ActionTypeID = NULL, ActionTypeCode = NULL, ActionID = NULL, Status = NULL, OrderedDate = NULL, DurationType = NULL, StaffIDAuthorizedBy = NULL, StaffIDFollowUpOfficer = NULL, LocationID = NULL, BuildingID = NULL, RoomID = NULL, TempIncidentOffenseNameID = NULL, IsGuardianNotified = NULL, StartTime = NULL, InternalComment = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameAction", objectId = TempIncidentOffenseNameActionID, body = list(DataObject = body), searchFields = append("TempIncidentOffenseNameActionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Incidents
	#'
	#' This function returns a dataframe or json object of Incidents
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Incidents. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Incidents.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Incident') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of Incidents
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listIncidents <- function(searchConditionsList = NULL, IncidentID = F, DistrictID = F, EntityID = F, SchoolYearID = F, IncidentNumber = F, DateTime = F, Type = F, LocationID = F, BuildingID = F, RoomID = F, Description = F, DamageCost = F, ActionIDRecommended = F, ReferredByType = F, ReferredByNameID = F, ReferredByFreeformName = F, ReferredByName = F, IncidentNumberValue = F, HasActions = F, HasActionsForOffenders = F, HasOpenActions = F, HasOverdueActionDetails = F, IsReferralOrWarning = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IncidentMNID = F, StateDIRSTimeMNID = F, ReportedToLawEnforcement = F, NumberOfNonEnrolledOffenders = F, NumberOfNonEnrolledVictims = F, IsIncidentOrWarning = F, IsSuppressed = F, HasWeapons = F, HasDrugs = F, IsReadOnlyHistoricalRecord = F, DateBeforeLastEffectiveRun = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "Incident", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an Incident
	#'
	#' This function returns a dataframe or json object of an Incident
	#' @param IncidentID The ID of the Incident to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Incident. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Incident.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Incident') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of Incident
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getIncident <- function(IncidentID, DistrictID = F, EntityID = F, SchoolYearID = F, IncidentNumber = F, DateTime = F, Type = F, LocationID = F, BuildingID = F, RoomID = F, Description = F, DamageCost = F, ActionIDRecommended = F, ReferredByType = F, ReferredByNameID = F, ReferredByFreeformName = F, ReferredByName = F, IncidentNumberValue = F, HasActions = F, HasActionsForOffenders = F, HasOpenActions = F, HasOverdueActionDetails = F, IsReferralOrWarning = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IncidentMNID = F, StateDIRSTimeMNID = F, ReportedToLawEnforcement = F, NumberOfNonEnrolledOffenders = F, NumberOfNonEnrolledVictims = F, IsIncidentOrWarning = F, IsSuppressed = F, HasWeapons = F, HasDrugs = F, IsReadOnlyHistoricalRecord = F, DateBeforeLastEffectiveRun = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "IncidentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "Incident", objectId = IncidentID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an Incident
	#'
	#' This function deletes an Incident
	#' @param IncidentID The ID of the Incident to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The IncidentID of the deleted Incident.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteIncident <- function(IncidentID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "Incident", objectId = IncidentID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an Incident
	#'
	#' This function creates an Incident
	#' @param fieldNames The field values to give the created Incident. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created Incident
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createIncident <- function(DistrictID = NULL, EntityID = NULL, SchoolYearID = NULL, IncidentNumber = NULL, DateTime = NULL, Type = NULL, LocationID = NULL, BuildingID = NULL, RoomID = NULL, Description = NULL, DamageCost = NULL, ActionIDRecommended = NULL, ReferredByType = NULL, ReferredByNameID = NULL, ReferredByFreeformName = NULL, StateDIRSTimeMNID = NULL, ReportedToLawEnforcement = NULL, NumberOfNonEnrolledOffenders = NULL, NumberOfNonEnrolledVictims = NULL, IsSuppressed = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "Incident", body = list(DataObject = body), searchFields = append("IncidentID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an Incident
	#'
	#' This function modifies an Incident
	#' @param fieldNames The field values to give the modified Incident. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified Incident
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyIncident <- function(IncidentID, DistrictID = NULL, EntityID = NULL, SchoolYearID = NULL, IncidentNumber = NULL, DateTime = NULL, Type = NULL, LocationID = NULL, BuildingID = NULL, RoomID = NULL, Description = NULL, DamageCost = NULL, ActionIDRecommended = NULL, ReferredByType = NULL, ReferredByNameID = NULL, ReferredByFreeformName = NULL, StateDIRSTimeMNID = NULL, ReportedToLawEnforcement = NULL, NumberOfNonEnrolledOffenders = NULL, NumberOfNonEnrolledVictims = NULL, IsSuppressed = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "Incident", objectId = IncidentID, body = list(DataObject = body), searchFields = append("IncidentID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List IncidentOffenses
	#'
	#' This function returns a dataframe or json object of IncidentOffenses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given IncidentOffenses. Defaults to FALSE for all return fields which, for convenience, returns all fields for the IncidentOffenses.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('IncidentOffense') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of IncidentOffenses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listIncidentOffenses <- function(searchConditionsList = NULL, IncidentOffenseID = F, IncidentID = F, OffenseID = F, IsPrimaryOffense = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasActions = F, HasWeapons = F, HasDrugs = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "IncidentOffense", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an IncidentOffense
	#'
	#' This function returns a dataframe or json object of an IncidentOffense
	#' @param IncidentOffenseID The ID of the IncidentOffense to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given IncidentOffense. Defaults to FALSE for all return fields which, for convenience, returns all fields for the IncidentOffense.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('IncidentOffense') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of IncidentOffense
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getIncidentOffense <- function(IncidentOffenseID, IncidentID = F, OffenseID = F, IsPrimaryOffense = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasActions = F, HasWeapons = F, HasDrugs = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "IncidentOffenseID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "IncidentOffense", objectId = IncidentOffenseID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an IncidentOffense
	#'
	#' This function deletes an IncidentOffense
	#' @param IncidentOffenseID The ID of the IncidentOffense to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The IncidentOffenseID of the deleted IncidentOffense.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteIncidentOffense <- function(IncidentOffenseID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "IncidentOffense", objectId = IncidentOffenseID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an IncidentOffense
	#'
	#' This function creates an IncidentOffense
	#' @param fieldNames The field values to give the created IncidentOffense. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created IncidentOffense
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createIncidentOffense <- function(IncidentID = NULL, OffenseID = NULL, IsPrimaryOffense = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "IncidentOffense", body = list(DataObject = body), searchFields = append("IncidentOffenseID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an IncidentOffense
	#'
	#' This function modifies an IncidentOffense
	#' @param fieldNames The field values to give the modified IncidentOffense. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified IncidentOffense
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyIncidentOffense <- function(IncidentOffenseID, IncidentID = NULL, OffenseID = NULL, IsPrimaryOffense = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "IncidentOffense", objectId = IncidentOffenseID, body = list(DataObject = body), searchFields = append("IncidentOffenseID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List IncidentOffenseNames
	#'
	#' This function returns a dataframe or json object of IncidentOffenseNames
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given IncidentOffenseNames. Defaults to FALSE for all return fields which, for convenience, returns all fields for the IncidentOffenseNames.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('IncidentOffenseName') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of IncidentOffenseNames
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listIncidentOffenseNames <- function(searchConditionsList = NULL, IncidentOffenseNameID = F, IncidentOffenseID = F, NameID = F, IncidentOffenseNameType = F, FreeformName = F, InvolvementType = F, StaffIDDisciplineOfficer = F, Statement = F, IsGuardianNotified = F, PerceivedMotivationID = F, OffenseLevelID = F, PersonalName = F, HasActions = F, HasOpenActions = F, HasOverdueActionDetails = F, IsStudentOffender = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IncidentOffenseNameMNID = F, StateVictimTypeMNID = F, StateVictimCostMNID = F, InjuryOccured = F, WasSeriousBodilyInjury = F, AttachmentCount = F, HasWeapons = F, HasDrugs = F, ReportedToLawEnforcement = F, StateOffenderActivityMNID = F, HasDangerousWeapons = F, IsPhysicalAssault = F, IsPhysicalAssaultState = F, FirstDrugCodeforNorthEastExport = F, OffenderArrestedByLawEnforcement = F, IsReadOnlyHistoricalRecord = F, DisciplineThresholdID = F, InternalComment = F, MultipleVictimCount = F, EstimatedVictimsEnrolled = F, EstimatedVictimsNotEnrolled = F, CreatedBeforeLastEffectiveRunAndWasWithinDelay = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "IncidentOffenseName", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an IncidentOffenseName
	#'
	#' This function returns a dataframe or json object of an IncidentOffenseName
	#' @param IncidentOffenseNameID The ID of the IncidentOffenseName to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given IncidentOffenseName. Defaults to FALSE for all return fields which, for convenience, returns all fields for the IncidentOffenseName.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('IncidentOffenseName') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of IncidentOffenseName
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getIncidentOffenseName <- function(IncidentOffenseNameID, IncidentOffenseID = F, NameID = F, IncidentOffenseNameType = F, FreeformName = F, InvolvementType = F, StaffIDDisciplineOfficer = F, Statement = F, IsGuardianNotified = F, PerceivedMotivationID = F, OffenseLevelID = F, PersonalName = F, HasActions = F, HasOpenActions = F, HasOverdueActionDetails = F, IsStudentOffender = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IncidentOffenseNameMNID = F, StateVictimTypeMNID = F, StateVictimCostMNID = F, InjuryOccured = F, WasSeriousBodilyInjury = F, AttachmentCount = F, HasWeapons = F, HasDrugs = F, ReportedToLawEnforcement = F, StateOffenderActivityMNID = F, HasDangerousWeapons = F, IsPhysicalAssault = F, IsPhysicalAssaultState = F, FirstDrugCodeforNorthEastExport = F, OffenderArrestedByLawEnforcement = F, IsReadOnlyHistoricalRecord = F, DisciplineThresholdID = F, InternalComment = F, MultipleVictimCount = F, EstimatedVictimsEnrolled = F, EstimatedVictimsNotEnrolled = F, CreatedBeforeLastEffectiveRunAndWasWithinDelay = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "IncidentOffenseNameID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "IncidentOffenseName", objectId = IncidentOffenseNameID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an IncidentOffenseName
	#'
	#' This function deletes an IncidentOffenseName
	#' @param IncidentOffenseNameID The ID of the IncidentOffenseName to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The IncidentOffenseNameID of the deleted IncidentOffenseName.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteIncidentOffenseName <- function(IncidentOffenseNameID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "IncidentOffenseName", objectId = IncidentOffenseNameID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an IncidentOffenseName
	#'
	#' This function creates an IncidentOffenseName
	#' @param fieldNames The field values to give the created IncidentOffenseName. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created IncidentOffenseName
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createIncidentOffenseName <- function(IncidentOffenseID = NULL, NameID = NULL, IncidentOffenseNameType = NULL, FreeformName = NULL, InvolvementType = NULL, StaffIDDisciplineOfficer = NULL, Statement = NULL, IsGuardianNotified = NULL, PerceivedMotivationID = NULL, OffenseLevelID = NULL, StateVictimTypeMNID = NULL, StateVictimCostMNID = NULL, InjuryOccured = NULL, WasSeriousBodilyInjury = NULL, ReportedToLawEnforcement = NULL, StateOffenderActivityMNID = NULL, IsPhysicalAssault = NULL, IsPhysicalAssaultState = NULL, OffenderArrestedByLawEnforcement = NULL, DisciplineThresholdID = NULL, MultipleVictimCount = NULL, EstimatedVictimsEnrolled = NULL, EstimatedVictimsNotEnrolled = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "IncidentOffenseName", body = list(DataObject = body), searchFields = append("IncidentOffenseNameID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an IncidentOffenseName
	#'
	#' This function modifies an IncidentOffenseName
	#' @param fieldNames The field values to give the modified IncidentOffenseName. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified IncidentOffenseName
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyIncidentOffenseName <- function(IncidentOffenseNameID, IncidentOffenseID = NULL, NameID = NULL, IncidentOffenseNameType = NULL, FreeformName = NULL, InvolvementType = NULL, StaffIDDisciplineOfficer = NULL, Statement = NULL, IsGuardianNotified = NULL, PerceivedMotivationID = NULL, OffenseLevelID = NULL, StateVictimTypeMNID = NULL, StateVictimCostMNID = NULL, InjuryOccured = NULL, WasSeriousBodilyInjury = NULL, ReportedToLawEnforcement = NULL, StateOffenderActivityMNID = NULL, IsPhysicalAssault = NULL, IsPhysicalAssaultState = NULL, OffenderArrestedByLawEnforcement = NULL, DisciplineThresholdID = NULL, MultipleVictimCount = NULL, EstimatedVictimsEnrolled = NULL, EstimatedVictimsNotEnrolled = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "IncidentOffenseName", objectId = IncidentOffenseNameID, body = list(DataObject = body), searchFields = append("IncidentOffenseNameID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List IncidentOffenseNameActions
	#'
	#' This function returns a dataframe or json object of IncidentOffenseNameActions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given IncidentOffenseNameActions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the IncidentOffenseNameActions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('IncidentOffenseNameAction') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of IncidentOffenseNameActions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listIncidentOffenseNameActions <- function(searchConditionsList = NULL, IncidentOffenseNameActionID = F, IncidentOffenseNameID = F, ActionID = F, Status = F, OrderedDate = F, DurationToServe = F, EntityID = F, LocationID = F, BuildingID = F, RoomID = F, StaffIDAuthorizedBy = F, StaffIDFollowUpOfficer = F, IsGuardianNotified = F, Comment = F, DurationServedOverride = F, DurationServed = F, ActualDurationServed = F, TotalDurationServed = F, DurationToServeWithLabel = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IncidentOffenseNameActionMNID = F, StateDIRSAESTypeMNID = F, DateExpulsionExclusionEnds = F, ReturnBeforeYearEnd = F, ExclusionThroughYearEnd = F, ExpulsionModified = F, ExpulsionThroughYearEnd = F, DIRSActionExplanation = F, FirstActionDetailDateNorthEastExport = F, LastActionDetailDateNorthEastExport = F, ActionTypeID = F, DurationType = F, StartTime = F, IsReadOnlyHistoricalRecord = F, InternalComment = F, NoServiceProvidedExplanation = F, MeetsLastRunDates = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "IncidentOffenseNameAction", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an IncidentOffenseNameAction
	#'
	#' This function returns a dataframe or json object of an IncidentOffenseNameAction
	#' @param IncidentOffenseNameActionID The ID of the IncidentOffenseNameAction to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given IncidentOffenseNameAction. Defaults to FALSE for all return fields which, for convenience, returns all fields for the IncidentOffenseNameAction.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('IncidentOffenseNameAction') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of IncidentOffenseNameAction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getIncidentOffenseNameAction <- function(IncidentOffenseNameActionID, IncidentOffenseNameID = F, ActionID = F, Status = F, OrderedDate = F, DurationToServe = F, EntityID = F, LocationID = F, BuildingID = F, RoomID = F, StaffIDAuthorizedBy = F, StaffIDFollowUpOfficer = F, IsGuardianNotified = F, Comment = F, DurationServedOverride = F, DurationServed = F, ActualDurationServed = F, TotalDurationServed = F, DurationToServeWithLabel = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IncidentOffenseNameActionMNID = F, StateDIRSAESTypeMNID = F, DateExpulsionExclusionEnds = F, ReturnBeforeYearEnd = F, ExclusionThroughYearEnd = F, ExpulsionModified = F, ExpulsionThroughYearEnd = F, DIRSActionExplanation = F, FirstActionDetailDateNorthEastExport = F, LastActionDetailDateNorthEastExport = F, ActionTypeID = F, DurationType = F, StartTime = F, IsReadOnlyHistoricalRecord = F, InternalComment = F, NoServiceProvidedExplanation = F, MeetsLastRunDates = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "IncidentOffenseNameActionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "IncidentOffenseNameAction", objectId = IncidentOffenseNameActionID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an IncidentOffenseNameAction
	#'
	#' This function deletes an IncidentOffenseNameAction
	#' @param IncidentOffenseNameActionID The ID of the IncidentOffenseNameAction to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The IncidentOffenseNameActionID of the deleted IncidentOffenseNameAction.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteIncidentOffenseNameAction <- function(IncidentOffenseNameActionID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "IncidentOffenseNameAction", objectId = IncidentOffenseNameActionID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an IncidentOffenseNameAction
	#'
	#' This function creates an IncidentOffenseNameAction
	#' @param fieldNames The field values to give the created IncidentOffenseNameAction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created IncidentOffenseNameAction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createIncidentOffenseNameAction <- function(IncidentOffenseNameID = NULL, ActionID = NULL, Status = NULL, OrderedDate = NULL, DurationToServe = NULL, EntityID = NULL, LocationID = NULL, BuildingID = NULL, RoomID = NULL, StaffIDAuthorizedBy = NULL, StaffIDFollowUpOfficer = NULL, IsGuardianNotified = NULL, Comment = NULL, DurationServedOverride = NULL, StateDIRSAESTypeMNID = NULL, DateExpulsionExclusionEnds = NULL, ReturnBeforeYearEnd = NULL, ExclusionThroughYearEnd = NULL, ExpulsionModified = NULL, ExpulsionThroughYearEnd = NULL, DIRSActionExplanation = NULL, ActionTypeID = NULL, DurationType = NULL, StartTime = NULL, NoServiceProvidedExplanation = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "IncidentOffenseNameAction", body = list(DataObject = body), searchFields = append("IncidentOffenseNameActionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an IncidentOffenseNameAction
	#'
	#' This function modifies an IncidentOffenseNameAction
	#' @param fieldNames The field values to give the modified IncidentOffenseNameAction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified IncidentOffenseNameAction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyIncidentOffenseNameAction <- function(IncidentOffenseNameActionID, IncidentOffenseNameID = NULL, ActionID = NULL, Status = NULL, OrderedDate = NULL, DurationToServe = NULL, EntityID = NULL, LocationID = NULL, BuildingID = NULL, RoomID = NULL, StaffIDAuthorizedBy = NULL, StaffIDFollowUpOfficer = NULL, IsGuardianNotified = NULL, Comment = NULL, DurationServedOverride = NULL, StateDIRSAESTypeMNID = NULL, DateExpulsionExclusionEnds = NULL, ReturnBeforeYearEnd = NULL, ExclusionThroughYearEnd = NULL, ExpulsionModified = NULL, ExpulsionThroughYearEnd = NULL, DIRSActionExplanation = NULL, ActionTypeID = NULL, DurationType = NULL, StartTime = NULL, NoServiceProvidedExplanation = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "IncidentOffenseNameAction", objectId = IncidentOffenseNameActionID, body = list(DataObject = body), searchFields = append("IncidentOffenseNameActionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List IncidentOffenseNameActionDetails
	#'
	#' This function returns a dataframe or json object of IncidentOffenseNameActionDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given IncidentOffenseNameActionDetails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the IncidentOffenseNameActionDetails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('IncidentOffenseNameActionDetail') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of IncidentOffenseNameActionDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listIncidentOffenseNameActionDetails <- function(searchConditionsList = NULL, IncidentOffenseNameActionDetailID = F, IncidentOffenseNameActionID = F, Status = F, ScheduledTime = F, IsAlternate = F, DurationToServe = F, DurationServed = F, LocationID = F, BuildingID = F, RoomID = F, StaffIDFollowUpOfficer = F, IsGuardianNotified = F, Comment = F, LastAlternate = F, RenderReissueOption = F, DurationToServeWithLabel = F, DurationServedWithLabel = F, Overdue = F, PartialDayPeriods = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, InternalComment = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "IncidentOffenseNameActionDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an IncidentOffenseNameActionDetail
	#'
	#' This function returns a dataframe or json object of an IncidentOffenseNameActionDetail
	#' @param IncidentOffenseNameActionDetailID The ID of the IncidentOffenseNameActionDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given IncidentOffenseNameActionDetail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the IncidentOffenseNameActionDetail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('IncidentOffenseNameActionDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of IncidentOffenseNameActionDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getIncidentOffenseNameActionDetail <- function(IncidentOffenseNameActionDetailID, IncidentOffenseNameActionID = F, Status = F, ScheduledTime = F, IsAlternate = F, DurationToServe = F, DurationServed = F, LocationID = F, BuildingID = F, RoomID = F, StaffIDFollowUpOfficer = F, IsGuardianNotified = F, Comment = F, LastAlternate = F, RenderReissueOption = F, DurationToServeWithLabel = F, DurationServedWithLabel = F, Overdue = F, PartialDayPeriods = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, InternalComment = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "IncidentOffenseNameActionDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "IncidentOffenseNameActionDetail", objectId = IncidentOffenseNameActionDetailID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an IncidentOffenseNameActionDetail
	#'
	#' This function deletes an IncidentOffenseNameActionDetail
	#' @param IncidentOffenseNameActionDetailID The ID of the IncidentOffenseNameActionDetail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The IncidentOffenseNameActionDetailID of the deleted IncidentOffenseNameActionDetail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteIncidentOffenseNameActionDetail <- function(IncidentOffenseNameActionDetailID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "IncidentOffenseNameActionDetail", objectId = IncidentOffenseNameActionDetailID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an IncidentOffenseNameActionDetail
	#'
	#' This function creates an IncidentOffenseNameActionDetail
	#' @param fieldNames The field values to give the created IncidentOffenseNameActionDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created IncidentOffenseNameActionDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createIncidentOffenseNameActionDetail <- function(IncidentOffenseNameActionID = NULL, Status = NULL, ScheduledTime = NULL, IsAlternate = NULL, DurationToServe = NULL, DurationServed = NULL, LocationID = NULL, BuildingID = NULL, RoomID = NULL, StaffIDFollowUpOfficer = NULL, IsGuardianNotified = NULL, Comment = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "IncidentOffenseNameActionDetail", body = list(DataObject = body), searchFields = append("IncidentOffenseNameActionDetailID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an IncidentOffenseNameActionDetail
	#'
	#' This function modifies an IncidentOffenseNameActionDetail
	#' @param fieldNames The field values to give the modified IncidentOffenseNameActionDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified IncidentOffenseNameActionDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyIncidentOffenseNameActionDetail <- function(IncidentOffenseNameActionDetailID, IncidentOffenseNameActionID = NULL, Status = NULL, ScheduledTime = NULL, IsAlternate = NULL, DurationToServe = NULL, DurationServed = NULL, LocationID = NULL, BuildingID = NULL, RoomID = NULL, StaffIDFollowUpOfficer = NULL, IsGuardianNotified = NULL, Comment = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "IncidentOffenseNameActionDetail", objectId = IncidentOffenseNameActionDetailID, body = list(DataObject = body), searchFields = append("IncidentOffenseNameActionDetailID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List NextIncidentNumbers
	#'
	#' This function returns a dataframe or json object of NextIncidentNumbers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NextIncidentNumbers. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NextIncidentNumbers.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NextIncidentNumber') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of NextIncidentNumbers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNextIncidentNumbers <- function(searchConditionsList = NULL, NextIncidentNumberID = F, DistrictID = F, SchoolYearID = F, SequenceNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "NextIncidentNumber", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NextIncidentNumber
	#'
	#' This function returns a dataframe or json object of a NextIncidentNumber
	#' @param NextIncidentNumberID The ID of the NextIncidentNumber to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NextIncidentNumber. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NextIncidentNumber.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NextIncidentNumber') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of NextIncidentNumber
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNextIncidentNumber <- function(NextIncidentNumberID, DistrictID = F, SchoolYearID = F, SequenceNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NextIncidentNumberID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "NextIncidentNumber", objectId = NextIncidentNumberID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NextIncidentNumber
	#'
	#' This function deletes a NextIncidentNumber
	#' @param NextIncidentNumberID The ID of the NextIncidentNumber to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The NextIncidentNumberID of the deleted NextIncidentNumber.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNextIncidentNumber <- function(NextIncidentNumberID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "NextIncidentNumber", objectId = NextIncidentNumberID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NextIncidentNumber
	#'
	#' This function creates a NextIncidentNumber
	#' @param fieldNames The field values to give the created NextIncidentNumber. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created NextIncidentNumber
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNextIncidentNumber <- function(DistrictID = NULL, SchoolYearID = NULL, SequenceNumber = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "NextIncidentNumber", body = list(DataObject = body), searchFields = append("NextIncidentNumberID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NextIncidentNumber
	#'
	#' This function modifies a NextIncidentNumber
	#' @param fieldNames The field values to give the modified NextIncidentNumber. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified NextIncidentNumber
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNextIncidentNumber <- function(NextIncidentNumberID, DistrictID = NULL, SchoolYearID = NULL, SequenceNumber = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "NextIncidentNumber", objectId = NextIncidentNumberID, body = list(DataObject = body), searchFields = append("NextIncidentNumberID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempIncidentOffenseNameActionDetailRecords
	#'
	#' This function returns a dataframe or json object of TempIncidentOffenseNameActionDetailRecords
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentOffenseNameActionDetailRecords. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentOffenseNameActionDetailRecords.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentOffenseNameActionDetailRecord') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of TempIncidentOffenseNameActionDetailRecords
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempIncidentOffenseNameActionDetailRecords <- function(searchConditionsList = NULL, TempIncidentOffenseNameActionDetailRecordID = F, IncidentOffenseNameActionID = F, StatusCode = F, Status = F, ScheduledTime = F, IsAlternate = F, DurationToServe = F, DurationServed = F, DurationType = F, LocationID = F, Location = F, BuildingID = F, Building = F, RoomID = F, RoomNumber = F, StaffIDFollowUpOfficer = F, StaffFollowUpOfficerName = F, IsGuardianNotified = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffenseNameActionDetailRecord", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempIncidentOffenseNameActionDetailRecord
	#'
	#' This function returns a dataframe or json object of a TempIncidentOffenseNameActionDetailRecord
	#' @param TempIncidentOffenseNameActionDetailRecordID The ID of the TempIncidentOffenseNameActionDetailRecord to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentOffenseNameActionDetailRecord. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentOffenseNameActionDetailRecord.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentOffenseNameActionDetailRecord') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of TempIncidentOffenseNameActionDetailRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempIncidentOffenseNameActionDetailRecord <- function(TempIncidentOffenseNameActionDetailRecordID, IncidentOffenseNameActionID = F, StatusCode = F, Status = F, ScheduledTime = F, IsAlternate = F, DurationToServe = F, DurationServed = F, DurationType = F, LocationID = F, Location = F, BuildingID = F, Building = F, RoomID = F, RoomNumber = F, StaffIDFollowUpOfficer = F, StaffFollowUpOfficerName = F, IsGuardianNotified = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempIncidentOffenseNameActionDetailRecordID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameActionDetailRecord", objectId = TempIncidentOffenseNameActionDetailRecordID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempIncidentOffenseNameActionDetailRecord
	#'
	#' This function deletes a TempIncidentOffenseNameActionDetailRecord
	#' @param TempIncidentOffenseNameActionDetailRecordID The ID of the TempIncidentOffenseNameActionDetailRecord to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The TempIncidentOffenseNameActionDetailRecordID of the deleted TempIncidentOffenseNameActionDetailRecord.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempIncidentOffenseNameActionDetailRecord <- function(TempIncidentOffenseNameActionDetailRecordID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameActionDetailRecord", objectId = TempIncidentOffenseNameActionDetailRecordID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempIncidentOffenseNameActionDetailRecord
	#'
	#' This function creates a TempIncidentOffenseNameActionDetailRecord
	#' @param fieldNames The field values to give the created TempIncidentOffenseNameActionDetailRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created TempIncidentOffenseNameActionDetailRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempIncidentOffenseNameActionDetailRecord <- function(IncidentOffenseNameActionID = NULL, StatusCode = NULL, Status = NULL, ScheduledTime = NULL, IsAlternate = NULL, DurationToServe = NULL, DurationServed = NULL, DurationType = NULL, LocationID = NULL, Location = NULL, BuildingID = NULL, Building = NULL, RoomID = NULL, RoomNumber = NULL, StaffIDFollowUpOfficer = NULL, StaffFollowUpOfficerName = NULL, IsGuardianNotified = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameActionDetailRecord", body = list(DataObject = body), searchFields = append("TempIncidentOffenseNameActionDetailRecordID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempIncidentOffenseNameActionDetailRecord
	#'
	#' This function modifies a TempIncidentOffenseNameActionDetailRecord
	#' @param fieldNames The field values to give the modified TempIncidentOffenseNameActionDetailRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified TempIncidentOffenseNameActionDetailRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempIncidentOffenseNameActionDetailRecord <- function(TempIncidentOffenseNameActionDetailRecordID, IncidentOffenseNameActionID = NULL, StatusCode = NULL, Status = NULL, ScheduledTime = NULL, IsAlternate = NULL, DurationToServe = NULL, DurationServed = NULL, DurationType = NULL, LocationID = NULL, Location = NULL, BuildingID = NULL, Building = NULL, RoomID = NULL, RoomNumber = NULL, StaffIDFollowUpOfficer = NULL, StaffFollowUpOfficerName = NULL, IsGuardianNotified = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameActionDetailRecord", objectId = TempIncidentOffenseNameActionDetailRecordID, body = list(DataObject = body), searchFields = append("TempIncidentOffenseNameActionDetailRecordID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DisciplineActions
	#'
	#' This function returns a dataframe or json object of DisciplineActions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DisciplineActions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DisciplineActions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DisciplineAction') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of DisciplineActions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDisciplineActions <- function(searchConditionsList = NULL, ActionID = F, DistrictID = F, SchoolYearID = F, Code = F, Description = F, DurationType = F, DefaultDuration = F, DefaultLocationID = F, SuppressCreationOfActionDetails = F, CodeDescription = F, CreateAttendanceForActionDetail = F, ActionIDClonedFrom = F, FederalDisciplineCategoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ActionMNID = F, StateActionTypeMNID = F, ActionTypeID = F, RestraintSeclusion = F, TransferToAlternativeSchool = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "Action", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DisciplineAction
	#'
	#' This function returns a dataframe or json object of a DisciplineAction
	#' @param DisciplineActionID The ID of the DisciplineAction to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DisciplineAction. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DisciplineAction.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DisciplineAction') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of DisciplineAction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDisciplineAction <- function(DisciplineActionID, ActionID = F, DistrictID = F, SchoolYearID = F, Code = F, Description = F, DurationType = F, DefaultDuration = F, DefaultLocationID = F, SuppressCreationOfActionDetails = F, CodeDescription = F, CreateAttendanceForActionDetail = F, ActionIDClonedFrom = F, FederalDisciplineCategoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ActionMNID = F, StateActionTypeMNID = F, ActionTypeID = F, RestraintSeclusion = F, TransferToAlternativeSchool = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DisciplineActionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "Action", objectId = DisciplineActionID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DisciplineAction
	#'
	#' This function deletes a DisciplineAction
	#' @param DisciplineActionID The ID of the DisciplineAction to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The DisciplineActionID of the deleted DisciplineAction.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDisciplineAction <- function(DisciplineActionID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "Action", objectId = DisciplineActionID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DisciplineAction
	#'
	#' This function creates a DisciplineAction
	#' @param fieldNames The field values to give the created DisciplineAction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created DisciplineAction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDisciplineAction <- function(DistrictID = NULL, SchoolYearID = NULL, Code = NULL, Description = NULL, DurationType = NULL, DefaultDuration = NULL, DefaultLocationID = NULL, SuppressCreationOfActionDetails = NULL, CreateAttendanceForActionDetail = NULL, ActionIDClonedFrom = NULL, FederalDisciplineCategoryID = NULL, StateActionTypeMNID = NULL, ActionTypeID = NULL, RestraintSeclusion = NULL, TransferToAlternativeSchool = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "Action", body = list(DataObject = body), searchFields = append("ActionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DisciplineAction
	#'
	#' This function modifies a DisciplineAction
	#' @param fieldNames The field values to give the modified DisciplineAction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified DisciplineAction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDisciplineAction <- function(ActionID, DistrictID = NULL, SchoolYearID = NULL, Code = NULL, Description = NULL, DurationType = NULL, DefaultDuration = NULL, DefaultLocationID = NULL, SuppressCreationOfActionDetails = NULL, CreateAttendanceForActionDetail = NULL, ActionIDClonedFrom = NULL, FederalDisciplineCategoryID = NULL, StateActionTypeMNID = NULL, ActionTypeID = NULL, RestraintSeclusion = NULL, TransferToAlternativeSchool = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "Action", objectId = ActionID, body = list(DataObject = body), searchFields = append("ActionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DisciplineConfigDistrictYears
	#'
	#' This function returns a dataframe or json object of DisciplineConfigDistrictYears
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DisciplineConfigDistrictYears. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DisciplineConfigDistrictYears.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DisciplineConfigDistrictYear') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of DisciplineConfigDistrictYears
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDisciplineConfigDistrictYears <- function(searchConditionsList = NULL, ConfigDistrictYearID = F, DistrictID = F, SchoolYearID = F, RestartIncidentNumberThisYear = F, AllowOnlyOneOffensePerIncident = F, DefaultDisciplineScreenDateAndTimes = F, DefaultOffenseValueFromPreviousPerson = F, DefaultActionValueFromPreviousPerson = F, UsePerceivedMotivation = F, UseIncidentBuildingAndRoom = F, ConfigDistrictYearIDClonedFrom = F, AllowUseOfWarning = F, AllowActionRecommendationsOnReferrals = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DisplayWarningsInFamilyAndStudentAccess = F, DaysToDelayDisplayOfIncidentsInFamilyAndStudentAccess = F, AllowActionTypeUpdate = F, DisplayStudentOffensesForAllEntities = F, DisplayInvolvedPersonsFromAllEntities = F, AllowDurationTypeUpdate = F, DefaultActionStatus = F, DefaultGuardianNotifiedOnActionDetailFromAction = F, IsReadOnlyHistoricalRecord = F, AllowInternalComments = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "ConfigDistrictYear", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DisciplineConfigDistrictYear
	#'
	#' This function returns a dataframe or json object of a DisciplineConfigDistrictYear
	#' @param DisciplineConfigDistrictYearID The ID of the DisciplineConfigDistrictYear to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DisciplineConfigDistrictYear. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DisciplineConfigDistrictYear.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DisciplineConfigDistrictYear') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of DisciplineConfigDistrictYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDisciplineConfigDistrictYear <- function(DisciplineConfigDistrictYearID, ConfigDistrictYearID = F, DistrictID = F, SchoolYearID = F, RestartIncidentNumberThisYear = F, AllowOnlyOneOffensePerIncident = F, DefaultDisciplineScreenDateAndTimes = F, DefaultOffenseValueFromPreviousPerson = F, DefaultActionValueFromPreviousPerson = F, UsePerceivedMotivation = F, UseIncidentBuildingAndRoom = F, ConfigDistrictYearIDClonedFrom = F, AllowUseOfWarning = F, AllowActionRecommendationsOnReferrals = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DisplayWarningsInFamilyAndStudentAccess = F, DaysToDelayDisplayOfIncidentsInFamilyAndStudentAccess = F, AllowActionTypeUpdate = F, DisplayStudentOffensesForAllEntities = F, DisplayInvolvedPersonsFromAllEntities = F, AllowDurationTypeUpdate = F, DefaultActionStatus = F, DefaultGuardianNotifiedOnActionDetailFromAction = F, IsReadOnlyHistoricalRecord = F, AllowInternalComments = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DisciplineConfigDistrictYearID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "ConfigDistrictYear", objectId = DisciplineConfigDistrictYearID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DisciplineConfigDistrictYear
	#'
	#' This function deletes a DisciplineConfigDistrictYear
	#' @param DisciplineConfigDistrictYearID The ID of the DisciplineConfigDistrictYear to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The DisciplineConfigDistrictYearID of the deleted DisciplineConfigDistrictYear.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDisciplineConfigDistrictYear <- function(DisciplineConfigDistrictYearID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "ConfigDistrictYear", objectId = DisciplineConfigDistrictYearID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DisciplineConfigDistrictYear
	#'
	#' This function creates a DisciplineConfigDistrictYear
	#' @param fieldNames The field values to give the created DisciplineConfigDistrictYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created DisciplineConfigDistrictYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDisciplineConfigDistrictYear <- function(DistrictID = NULL, SchoolYearID = NULL, RestartIncidentNumberThisYear = NULL, AllowOnlyOneOffensePerIncident = NULL, DefaultDisciplineScreenDateAndTimes = NULL, DefaultOffenseValueFromPreviousPerson = NULL, DefaultActionValueFromPreviousPerson = NULL, UsePerceivedMotivation = NULL, UseIncidentBuildingAndRoom = NULL, ConfigDistrictYearIDClonedFrom = NULL, AllowUseOfWarning = NULL, AllowActionRecommendationsOnReferrals = NULL, DisplayWarningsInFamilyAndStudentAccess = NULL, DaysToDelayDisplayOfIncidentsInFamilyAndStudentAccess = NULL, AllowActionTypeUpdate = NULL, DisplayStudentOffensesForAllEntities = NULL, DisplayInvolvedPersonsFromAllEntities = NULL, AllowDurationTypeUpdate = NULL, DefaultActionStatus = NULL, DefaultGuardianNotifiedOnActionDetailFromAction = NULL, AllowInternalComments = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "ConfigDistrictYear", body = list(DataObject = body), searchFields = append("ConfigDistrictYearID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DisciplineConfigDistrictYear
	#'
	#' This function modifies a DisciplineConfigDistrictYear
	#' @param fieldNames The field values to give the modified DisciplineConfigDistrictYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified DisciplineConfigDistrictYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDisciplineConfigDistrictYear <- function(ConfigDistrictYearID, DistrictID = NULL, SchoolYearID = NULL, RestartIncidentNumberThisYear = NULL, AllowOnlyOneOffensePerIncident = NULL, DefaultDisciplineScreenDateAndTimes = NULL, DefaultOffenseValueFromPreviousPerson = NULL, DefaultActionValueFromPreviousPerson = NULL, UsePerceivedMotivation = NULL, UseIncidentBuildingAndRoom = NULL, ConfigDistrictYearIDClonedFrom = NULL, AllowUseOfWarning = NULL, AllowActionRecommendationsOnReferrals = NULL, DisplayWarningsInFamilyAndStudentAccess = NULL, DaysToDelayDisplayOfIncidentsInFamilyAndStudentAccess = NULL, AllowActionTypeUpdate = NULL, DisplayStudentOffensesForAllEntities = NULL, DisplayInvolvedPersonsFromAllEntities = NULL, AllowDurationTypeUpdate = NULL, DefaultActionStatus = NULL, DefaultGuardianNotifiedOnActionDetailFromAction = NULL, AllowInternalComments = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "ConfigDistrictYear", objectId = ConfigDistrictYearID, body = list(DataObject = body), searchFields = append("ConfigDistrictYearID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PerceivedMotivations
	#'
	#' This function returns a dataframe or json object of PerceivedMotivations
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PerceivedMotivations. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PerceivedMotivations.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PerceivedMotivation') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of PerceivedMotivations
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPerceivedMotivations <- function(searchConditionsList = NULL, PerceivedMotivationID = F, DistrictID = F, SchoolYearID = F, Code = F, Description = F, CodeDescription = F, PerceivedMotivationIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "PerceivedMotivation", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PerceivedMotivation
	#'
	#' This function returns a dataframe or json object of a PerceivedMotivation
	#' @param PerceivedMotivationID The ID of the PerceivedMotivation to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PerceivedMotivation. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PerceivedMotivation.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PerceivedMotivation') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of PerceivedMotivation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPerceivedMotivation <- function(PerceivedMotivationID, DistrictID = F, SchoolYearID = F, Code = F, Description = F, CodeDescription = F, PerceivedMotivationIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PerceivedMotivationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "PerceivedMotivation", objectId = PerceivedMotivationID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PerceivedMotivation
	#'
	#' This function deletes a PerceivedMotivation
	#' @param PerceivedMotivationID The ID of the PerceivedMotivation to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The PerceivedMotivationID of the deleted PerceivedMotivation.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePerceivedMotivation <- function(PerceivedMotivationID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "PerceivedMotivation", objectId = PerceivedMotivationID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PerceivedMotivation
	#'
	#' This function creates a PerceivedMotivation
	#' @param fieldNames The field values to give the created PerceivedMotivation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created PerceivedMotivation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPerceivedMotivation <- function(DistrictID = NULL, SchoolYearID = NULL, Code = NULL, Description = NULL, PerceivedMotivationIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "PerceivedMotivation", body = list(DataObject = body), searchFields = append("PerceivedMotivationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PerceivedMotivation
	#'
	#' This function modifies a PerceivedMotivation
	#' @param fieldNames The field values to give the modified PerceivedMotivation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified PerceivedMotivation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPerceivedMotivation <- function(PerceivedMotivationID, DistrictID = NULL, SchoolYearID = NULL, Code = NULL, Description = NULL, PerceivedMotivationIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "PerceivedMotivation", objectId = PerceivedMotivationID, body = list(DataObject = body), searchFields = append("PerceivedMotivationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DisciplineLocations
	#'
	#' This function returns a dataframe or json object of DisciplineLocations
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DisciplineLocations. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DisciplineLocations.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DisciplineLocation') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of DisciplineLocations
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDisciplineLocations <- function(searchConditionsList = NULL, LocationID = F, DistrictID = F, Code = F, Description = F, EdFiIncidentLocationID = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, LocationMNID = F, StateIncidentLocationMNID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "Location", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DisciplineLocation
	#'
	#' This function returns a dataframe or json object of a DisciplineLocation
	#' @param DisciplineLocationID The ID of the DisciplineLocation to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DisciplineLocation. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DisciplineLocation.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DisciplineLocation') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of DisciplineLocation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDisciplineLocation <- function(DisciplineLocationID, LocationID = F, DistrictID = F, Code = F, Description = F, EdFiIncidentLocationID = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, LocationMNID = F, StateIncidentLocationMNID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DisciplineLocationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "Location", objectId = DisciplineLocationID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DisciplineLocation
	#'
	#' This function deletes a DisciplineLocation
	#' @param DisciplineLocationID The ID of the DisciplineLocation to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The DisciplineLocationID of the deleted DisciplineLocation.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDisciplineLocation <- function(DisciplineLocationID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "Location", objectId = DisciplineLocationID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DisciplineLocation
	#'
	#' This function creates a DisciplineLocation
	#' @param fieldNames The field values to give the created DisciplineLocation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created DisciplineLocation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDisciplineLocation <- function(DistrictID = NULL, Code = NULL, Description = NULL, EdFiIncidentLocationID = NULL, StateIncidentLocationMNID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "Location", body = list(DataObject = body), searchFields = append("LocationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DisciplineLocation
	#'
	#' This function modifies a DisciplineLocation
	#' @param fieldNames The field values to give the modified DisciplineLocation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified DisciplineLocation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDisciplineLocation <- function(LocationID, DistrictID = NULL, Code = NULL, Description = NULL, EdFiIncidentLocationID = NULL, StateIncidentLocationMNID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "Location", objectId = LocationID, body = list(DataObject = body), searchFields = append("LocationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Offenses
	#'
	#' This function returns a dataframe or json object of Offenses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Offenses. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Offenses.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Offense') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of Offenses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listOffenses <- function(searchConditionsList = NULL, OffenseID = F, DistrictID = F, SchoolYearID = F, Code = F, Description = F, RestrictActions = F, DefaultActionID = F, UseForReferral = F, AllowActionRecommendations = F, OffenseLevelIDDefault = F, CodeDescription = F, OffenseIDClonedFrom = F, FederalOffenseCategoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsDrugRelated = F, IsWeaponRelated = F, IsInjuryThreat = F, HarassmentBullying = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "Offense", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an Offense
	#'
	#' This function returns a dataframe or json object of an Offense
	#' @param OffenseID The ID of the Offense to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Offense. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Offense.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Offense') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of Offense
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getOffense <- function(OffenseID, DistrictID = F, SchoolYearID = F, Code = F, Description = F, RestrictActions = F, DefaultActionID = F, UseForReferral = F, AllowActionRecommendations = F, OffenseLevelIDDefault = F, CodeDescription = F, OffenseIDClonedFrom = F, FederalOffenseCategoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsDrugRelated = F, IsWeaponRelated = F, IsInjuryThreat = F, HarassmentBullying = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "OffenseID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "Offense", objectId = OffenseID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an Offense
	#'
	#' This function deletes an Offense
	#' @param OffenseID The ID of the Offense to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The OffenseID of the deleted Offense.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteOffense <- function(OffenseID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "Offense", objectId = OffenseID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an Offense
	#'
	#' This function creates an Offense
	#' @param fieldNames The field values to give the created Offense. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created Offense
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createOffense <- function(DistrictID = NULL, SchoolYearID = NULL, Code = NULL, Description = NULL, RestrictActions = NULL, DefaultActionID = NULL, UseForReferral = NULL, AllowActionRecommendations = NULL, OffenseLevelIDDefault = NULL, OffenseIDClonedFrom = NULL, FederalOffenseCategoryID = NULL, IsDrugRelated = NULL, IsWeaponRelated = NULL, IsInjuryThreat = NULL, HarassmentBullying = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "Offense", body = list(DataObject = body), searchFields = append("OffenseID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an Offense
	#'
	#' This function modifies an Offense
	#' @param fieldNames The field values to give the modified Offense. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified Offense
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyOffense <- function(OffenseID, DistrictID = NULL, SchoolYearID = NULL, Code = NULL, Description = NULL, RestrictActions = NULL, DefaultActionID = NULL, UseForReferral = NULL, AllowActionRecommendations = NULL, OffenseLevelIDDefault = NULL, OffenseIDClonedFrom = NULL, FederalOffenseCategoryID = NULL, IsDrugRelated = NULL, IsWeaponRelated = NULL, IsInjuryThreat = NULL, HarassmentBullying = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "Offense", objectId = OffenseID, body = list(DataObject = body), searchFields = append("OffenseID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List OffenseActions
	#'
	#' This function returns a dataframe or json object of OffenseActions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OffenseActions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OffenseActions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OffenseAction') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of OffenseActions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listOffenseActions <- function(searchConditionsList = NULL, OffenseActionID = F, OffenseID = F, ActionID = F, OffenseActionIDClonedFrom = F, IsReferralAction = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "OffenseAction", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an OffenseAction
	#'
	#' This function returns a dataframe or json object of an OffenseAction
	#' @param OffenseActionID The ID of the OffenseAction to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OffenseAction. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OffenseAction.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OffenseAction') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of OffenseAction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getOffenseAction <- function(OffenseActionID, OffenseID = F, ActionID = F, OffenseActionIDClonedFrom = F, IsReferralAction = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "OffenseActionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "OffenseAction", objectId = OffenseActionID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an OffenseAction
	#'
	#' This function deletes an OffenseAction
	#' @param OffenseActionID The ID of the OffenseAction to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The OffenseActionID of the deleted OffenseAction.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteOffenseAction <- function(OffenseActionID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "OffenseAction", objectId = OffenseActionID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an OffenseAction
	#'
	#' This function creates an OffenseAction
	#' @param fieldNames The field values to give the created OffenseAction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created OffenseAction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createOffenseAction <- function(OffenseID = NULL, ActionID = NULL, OffenseActionIDClonedFrom = NULL, IsReferralAction = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "OffenseAction", body = list(DataObject = body), searchFields = append("OffenseActionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an OffenseAction
	#'
	#' This function modifies an OffenseAction
	#' @param fieldNames The field values to give the modified OffenseAction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified OffenseAction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyOffenseAction <- function(OffenseActionID, OffenseID = NULL, ActionID = NULL, OffenseActionIDClonedFrom = NULL, IsReferralAction = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "OffenseAction", objectId = OffenseActionID, body = list(DataObject = body), searchFields = append("OffenseActionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List OffenseLevels
	#'
	#' This function returns a dataframe or json object of OffenseLevels
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OffenseLevels. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OffenseLevels.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OffenseLevel') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of OffenseLevels
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listOffenseLevels <- function(searchConditionsList = NULL, OffenseLevelID = F, DistrictID = F, SchoolYearID = F, Code = F, Description = F, CodeDescription = F, OffenseLevelIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "OffenseLevel", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an OffenseLevel
	#'
	#' This function returns a dataframe or json object of an OffenseLevel
	#' @param OffenseLevelID The ID of the OffenseLevel to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OffenseLevel. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OffenseLevel.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OffenseLevel') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of OffenseLevel
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getOffenseLevel <- function(OffenseLevelID, DistrictID = F, SchoolYearID = F, Code = F, Description = F, CodeDescription = F, OffenseLevelIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "OffenseLevelID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "OffenseLevel", objectId = OffenseLevelID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an OffenseLevel
	#'
	#' This function deletes an OffenseLevel
	#' @param OffenseLevelID The ID of the OffenseLevel to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The OffenseLevelID of the deleted OffenseLevel.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteOffenseLevel <- function(OffenseLevelID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "OffenseLevel", objectId = OffenseLevelID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an OffenseLevel
	#'
	#' This function creates an OffenseLevel
	#' @param fieldNames The field values to give the created OffenseLevel. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created OffenseLevel
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createOffenseLevel <- function(DistrictID = NULL, SchoolYearID = NULL, Code = NULL, Description = NULL, OffenseLevelIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "OffenseLevel", body = list(DataObject = body), searchFields = append("OffenseLevelID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an OffenseLevel
	#'
	#' This function modifies an OffenseLevel
	#' @param fieldNames The field values to give the modified OffenseLevel. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified OffenseLevel
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyOffenseLevel <- function(OffenseLevelID, DistrictID = NULL, SchoolYearID = NULL, Code = NULL, Description = NULL, OffenseLevelIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "OffenseLevel", objectId = OffenseLevelID, body = list(DataObject = body), searchFields = append("OffenseLevelID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Drugs
	#'
	#' This function returns a dataframe or json object of Drugs
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Drugs. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Drugs.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Drug') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of Drugs
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDrugs <- function(searchConditionsList = NULL, DrugMNID = F, StateDrugTypeMNID = F, DrugID = F, DistrictID = F, SchoolYearID = F, Code = F, Description = F, DrugIDClonedFrom = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "Drug", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Drug
	#'
	#' This function returns a dataframe or json object of a Drug
	#' @param DrugID The ID of the Drug to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Drug. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Drug.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Drug') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of Drug
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDrug <- function(DrugID, DrugMNID = F, StateDrugTypeMNID = F, DistrictID = F, SchoolYearID = F, Code = F, Description = F, DrugIDClonedFrom = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DrugID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "Drug", objectId = DrugID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Drug
	#'
	#' This function deletes a Drug
	#' @param DrugID The ID of the Drug to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The DrugID of the deleted Drug.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDrug <- function(DrugID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "Drug", objectId = DrugID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Drug
	#'
	#' This function creates a Drug
	#' @param fieldNames The field values to give the created Drug. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created Drug
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDrug <- function(StateDrugTypeMNID = NULL, DistrictID = NULL, SchoolYearID = NULL, Code = NULL, Description = NULL, DrugIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "Drug", body = list(DataObject = body), searchFields = append("DrugID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Drug
	#'
	#' This function modifies a Drug
	#' @param fieldNames The field values to give the modified Drug. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified Drug
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDrug <- function(DrugID, StateDrugTypeMNID = NULL, DistrictID = NULL, SchoolYearID = NULL, Code = NULL, Description = NULL, DrugIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "Drug", objectId = DrugID, body = list(DataObject = body), searchFields = append("DrugID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List IncidentOffenseNameDrugs
	#'
	#' This function returns a dataframe or json object of IncidentOffenseNameDrugs
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given IncidentOffenseNameDrugs. Defaults to FALSE for all return fields which, for convenience, returns all fields for the IncidentOffenseNameDrugs.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('IncidentOffenseNameDrug') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of IncidentOffenseNameDrugs
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listIncidentOffenseNameDrugs <- function(searchConditionsList = NULL, IncidentOffenseNameDrugID = F, IncidentOffenseNameID = F, DrugID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "IncidentOffenseNameDrug", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an IncidentOffenseNameDrug
	#'
	#' This function returns a dataframe or json object of an IncidentOffenseNameDrug
	#' @param IncidentOffenseNameDrugID The ID of the IncidentOffenseNameDrug to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given IncidentOffenseNameDrug. Defaults to FALSE for all return fields which, for convenience, returns all fields for the IncidentOffenseNameDrug.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('IncidentOffenseNameDrug') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of IncidentOffenseNameDrug
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getIncidentOffenseNameDrug <- function(IncidentOffenseNameDrugID, IncidentOffenseNameID = F, DrugID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "IncidentOffenseNameDrugID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "IncidentOffenseNameDrug", objectId = IncidentOffenseNameDrugID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an IncidentOffenseNameDrug
	#'
	#' This function deletes an IncidentOffenseNameDrug
	#' @param IncidentOffenseNameDrugID The ID of the IncidentOffenseNameDrug to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The IncidentOffenseNameDrugID of the deleted IncidentOffenseNameDrug.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteIncidentOffenseNameDrug <- function(IncidentOffenseNameDrugID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "IncidentOffenseNameDrug", objectId = IncidentOffenseNameDrugID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an IncidentOffenseNameDrug
	#'
	#' This function creates an IncidentOffenseNameDrug
	#' @param fieldNames The field values to give the created IncidentOffenseNameDrug. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created IncidentOffenseNameDrug
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createIncidentOffenseNameDrug <- function(IncidentOffenseNameID = NULL, DrugID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "IncidentOffenseNameDrug", body = list(DataObject = body), searchFields = append("IncidentOffenseNameDrugID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an IncidentOffenseNameDrug
	#'
	#' This function modifies an IncidentOffenseNameDrug
	#' @param fieldNames The field values to give the modified IncidentOffenseNameDrug. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified IncidentOffenseNameDrug
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyIncidentOffenseNameDrug <- function(IncidentOffenseNameDrugID, IncidentOffenseNameID = NULL, DrugID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "IncidentOffenseNameDrug", objectId = IncidentOffenseNameDrugID, body = list(DataObject = body), searchFields = append("IncidentOffenseNameDrugID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List IncidentOffenseNameWeapons
	#'
	#' This function returns a dataframe or json object of IncidentOffenseNameWeapons
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given IncidentOffenseNameWeapons. Defaults to FALSE for all return fields which, for convenience, returns all fields for the IncidentOffenseNameWeapons.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('IncidentOffenseNameWeapon') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of IncidentOffenseNameWeapons
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listIncidentOffenseNameWeapons <- function(searchConditionsList = NULL, IncidentOffenseNameWeaponID = F, IncidentOffenseNameID = F, WeaponID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IncidentOffenseNameWeaponMNID = F, MeetsFederalStatuteOfDangerousWeapon = F, MeetsStateStatuteOfDangerousWeapon = F, GunWasLoaded = F, GunWasInCase = F, GunFoundInTrunk = F, WeaponCount = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "IncidentOffenseNameWeapon", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an IncidentOffenseNameWeapon
	#'
	#' This function returns a dataframe or json object of an IncidentOffenseNameWeapon
	#' @param IncidentOffenseNameWeaponID The ID of the IncidentOffenseNameWeapon to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given IncidentOffenseNameWeapon. Defaults to FALSE for all return fields which, for convenience, returns all fields for the IncidentOffenseNameWeapon.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('IncidentOffenseNameWeapon') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of IncidentOffenseNameWeapon
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getIncidentOffenseNameWeapon <- function(IncidentOffenseNameWeaponID, IncidentOffenseNameID = F, WeaponID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IncidentOffenseNameWeaponMNID = F, MeetsFederalStatuteOfDangerousWeapon = F, MeetsStateStatuteOfDangerousWeapon = F, GunWasLoaded = F, GunWasInCase = F, GunFoundInTrunk = F, WeaponCount = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "IncidentOffenseNameWeaponID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "IncidentOffenseNameWeapon", objectId = IncidentOffenseNameWeaponID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an IncidentOffenseNameWeapon
	#'
	#' This function deletes an IncidentOffenseNameWeapon
	#' @param IncidentOffenseNameWeaponID The ID of the IncidentOffenseNameWeapon to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The IncidentOffenseNameWeaponID of the deleted IncidentOffenseNameWeapon.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteIncidentOffenseNameWeapon <- function(IncidentOffenseNameWeaponID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "IncidentOffenseNameWeapon", objectId = IncidentOffenseNameWeaponID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an IncidentOffenseNameWeapon
	#'
	#' This function creates an IncidentOffenseNameWeapon
	#' @param fieldNames The field values to give the created IncidentOffenseNameWeapon. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created IncidentOffenseNameWeapon
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createIncidentOffenseNameWeapon <- function(IncidentOffenseNameID = NULL, WeaponID = NULL, MeetsFederalStatuteOfDangerousWeapon = NULL, MeetsStateStatuteOfDangerousWeapon = NULL, GunWasLoaded = NULL, GunWasInCase = NULL, GunFoundInTrunk = NULL, WeaponCount = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "IncidentOffenseNameWeapon", body = list(DataObject = body), searchFields = append("IncidentOffenseNameWeaponID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an IncidentOffenseNameWeapon
	#'
	#' This function modifies an IncidentOffenseNameWeapon
	#' @param fieldNames The field values to give the modified IncidentOffenseNameWeapon. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified IncidentOffenseNameWeapon
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyIncidentOffenseNameWeapon <- function(IncidentOffenseNameWeaponID, IncidentOffenseNameID = NULL, WeaponID = NULL, MeetsFederalStatuteOfDangerousWeapon = NULL, MeetsStateStatuteOfDangerousWeapon = NULL, GunWasLoaded = NULL, GunWasInCase = NULL, GunFoundInTrunk = NULL, WeaponCount = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "IncidentOffenseNameWeapon", objectId = IncidentOffenseNameWeaponID, body = list(DataObject = body), searchFields = append("IncidentOffenseNameWeaponID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Weapons
	#'
	#' This function returns a dataframe or json object of Weapons
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Weapons. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Weapons.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Weapon') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of Weapons
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listWeapons <- function(searchConditionsList = NULL, WeaponMNID = F, StateWeaponTypeMNID = F, StatePelletGunTypeMNID = F, WeaponID = F, DistrictID = F, SchoolYearID = F, Code = F, Description = F, WeaponIDClonedFrom = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "Weapon", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Weapon
	#'
	#' This function returns a dataframe or json object of a Weapon
	#' @param WeaponID The ID of the Weapon to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Weapon. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Weapon.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Weapon') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of Weapon
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getWeapon <- function(WeaponID, WeaponMNID = F, StateWeaponTypeMNID = F, StatePelletGunTypeMNID = F, DistrictID = F, SchoolYearID = F, Code = F, Description = F, WeaponIDClonedFrom = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "WeaponID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "Weapon", objectId = WeaponID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Weapon
	#'
	#' This function deletes a Weapon
	#' @param WeaponID The ID of the Weapon to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The WeaponID of the deleted Weapon.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteWeapon <- function(WeaponID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "Weapon", objectId = WeaponID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Weapon
	#'
	#' This function creates a Weapon
	#' @param fieldNames The field values to give the created Weapon. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created Weapon
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createWeapon <- function(StateWeaponTypeMNID = NULL, StatePelletGunTypeMNID = NULL, DistrictID = NULL, SchoolYearID = NULL, Code = NULL, Description = NULL, WeaponIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "Weapon", body = list(DataObject = body), searchFields = append("WeaponID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Weapon
	#'
	#' This function modifies a Weapon
	#' @param fieldNames The field values to give the modified Weapon. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified Weapon
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyWeapon <- function(WeaponID, StateWeaponTypeMNID = NULL, StatePelletGunTypeMNID = NULL, DistrictID = NULL, SchoolYearID = NULL, Code = NULL, Description = NULL, WeaponIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "Weapon", objectId = WeaponID, body = list(DataObject = body), searchFields = append("WeaponID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ActionTypes
	#'
	#' This function returns a dataframe or json object of ActionTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ActionTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ActionTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ActionType') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of ActionTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listActionTypes <- function(searchConditionsList = NULL, ActionTypeID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, EdFiDisciplineDescriptorID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "ActionType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ActionType
	#'
	#' This function returns a dataframe or json object of an ActionType
	#' @param ActionTypeID The ID of the ActionType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ActionType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ActionType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ActionType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of ActionType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getActionType <- function(ActionTypeID, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, EdFiDisciplineDescriptorID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ActionTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "ActionType", objectId = ActionTypeID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ActionType
	#'
	#' This function deletes an ActionType
	#' @param ActionTypeID The ID of the ActionType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The ActionTypeID of the deleted ActionType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteActionType <- function(ActionTypeID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "ActionType", objectId = ActionTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ActionType
	#'
	#' This function creates an ActionType
	#' @param fieldNames The field values to give the created ActionType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created ActionType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createActionType <- function(Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, EdFiDisciplineDescriptorID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "ActionType", body = list(DataObject = body), searchFields = append("ActionTypeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ActionType
	#'
	#' This function modifies an ActionType
	#' @param fieldNames The field values to give the modified ActionType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified ActionType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyActionType <- function(ActionTypeID, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, EdFiDisciplineDescriptorID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "ActionType", objectId = ActionTypeID, body = list(DataObject = body), searchFields = append("ActionTypeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DisciplineConfigSystems
	#'
	#' This function returns a dataframe or json object of DisciplineConfigSystems
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DisciplineConfigSystems. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DisciplineConfigSystems.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DisciplineConfigSystem') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of DisciplineConfigSystems
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDisciplineConfigSystems <- function(searchConditionsList = NULL, ConfigSystemID = F, AllowIncidentSuppression = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ReportIDDisciplineLetter = F, AllowUpdateHistoricalData = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "ConfigSystem", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DisciplineConfigSystem
	#'
	#' This function returns a dataframe or json object of a DisciplineConfigSystem
	#' @param DisciplineConfigSystemID The ID of the DisciplineConfigSystem to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DisciplineConfigSystem. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DisciplineConfigSystem.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DisciplineConfigSystem') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of DisciplineConfigSystem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDisciplineConfigSystem <- function(DisciplineConfigSystemID, ConfigSystemID = F, AllowIncidentSuppression = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ReportIDDisciplineLetter = F, AllowUpdateHistoricalData = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DisciplineConfigSystemID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "ConfigSystem", objectId = DisciplineConfigSystemID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DisciplineConfigSystem
	#'
	#' This function deletes a DisciplineConfigSystem
	#' @param DisciplineConfigSystemID The ID of the DisciplineConfigSystem to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The DisciplineConfigSystemID of the deleted DisciplineConfigSystem.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDisciplineConfigSystem <- function(DisciplineConfigSystemID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "ConfigSystem", objectId = DisciplineConfigSystemID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DisciplineConfigSystem
	#'
	#' This function creates a DisciplineConfigSystem
	#' @param fieldNames The field values to give the created DisciplineConfigSystem. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created DisciplineConfigSystem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDisciplineConfigSystem <- function(AllowIncidentSuppression = NULL, ReportIDDisciplineLetter = NULL, AllowUpdateHistoricalData = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "ConfigSystem", body = list(DataObject = body), searchFields = append("ConfigSystemID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DisciplineConfigSystem
	#'
	#' This function modifies a DisciplineConfigSystem
	#' @param fieldNames The field values to give the modified DisciplineConfigSystem. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified DisciplineConfigSystem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDisciplineConfigSystem <- function(ConfigSystemID, AllowIncidentSuppression = NULL, ReportIDDisciplineLetter = NULL, AllowUpdateHistoricalData = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "ConfigSystem", objectId = ConfigSystemID, body = list(DataObject = body), searchFields = append("ConfigSystemID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DisciplineLetterTemplates
	#'
	#' This function returns a dataframe or json object of DisciplineLetterTemplates
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DisciplineLetterTemplates. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DisciplineLetterTemplates.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DisciplineLetterTemplate') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of DisciplineLetterTemplates
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDisciplineLetterTemplates <- function(searchConditionsList = NULL, DisciplineLetterTemplateID = F, Description = F, Header = F, Body = F, Footer = F, DistrictID = F, SchoolYearID = F, DisciplineLetterTemplateIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, MediaIDLogo = F, GuardianNameFormat = F, StudentNameFormat = F, AdvisorNameFormat = F, SuperintendentNameFormat = F, ColumnHeaderLabel1 = F, ColumnHeaderLabel2 = F, ColumnHeaderLabel3 = F, ColumnHeaderLabel4 = F, ColumnHeaderLabel5 = F, ColumnHeaderLabel6 = F, ColumnHeaderLabel7 = F, ColumnHeaderLabel8 = F, ColumnHeaderLabel9 = F, ColumnHeaderLabel10 = F, IsDefault = F, ForCurrentEntity = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "DisciplineLetterTemplate", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DisciplineLetterTemplate
	#'
	#' This function returns a dataframe or json object of a DisciplineLetterTemplate
	#' @param DisciplineLetterTemplateID The ID of the DisciplineLetterTemplate to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DisciplineLetterTemplate. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DisciplineLetterTemplate.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DisciplineLetterTemplate') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of DisciplineLetterTemplate
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDisciplineLetterTemplate <- function(DisciplineLetterTemplateID, Description = F, Header = F, Body = F, Footer = F, DistrictID = F, SchoolYearID = F, DisciplineLetterTemplateIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, MediaIDLogo = F, GuardianNameFormat = F, StudentNameFormat = F, AdvisorNameFormat = F, SuperintendentNameFormat = F, ColumnHeaderLabel1 = F, ColumnHeaderLabel2 = F, ColumnHeaderLabel3 = F, ColumnHeaderLabel4 = F, ColumnHeaderLabel5 = F, ColumnHeaderLabel6 = F, ColumnHeaderLabel7 = F, ColumnHeaderLabel8 = F, ColumnHeaderLabel9 = F, ColumnHeaderLabel10 = F, IsDefault = F, ForCurrentEntity = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DisciplineLetterTemplateID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "DisciplineLetterTemplate", objectId = DisciplineLetterTemplateID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DisciplineLetterTemplate
	#'
	#' This function deletes a DisciplineLetterTemplate
	#' @param DisciplineLetterTemplateID The ID of the DisciplineLetterTemplate to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The DisciplineLetterTemplateID of the deleted DisciplineLetterTemplate.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDisciplineLetterTemplate <- function(DisciplineLetterTemplateID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "DisciplineLetterTemplate", objectId = DisciplineLetterTemplateID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DisciplineLetterTemplate
	#'
	#' This function creates a DisciplineLetterTemplate
	#' @param fieldNames The field values to give the created DisciplineLetterTemplate. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created DisciplineLetterTemplate
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDisciplineLetterTemplate <- function(Description = NULL, Header = NULL, Body = NULL, Footer = NULL, DistrictID = NULL, SchoolYearID = NULL, DisciplineLetterTemplateIDClonedFrom = NULL, MediaIDLogo = NULL, GuardianNameFormat = NULL, StudentNameFormat = NULL, AdvisorNameFormat = NULL, SuperintendentNameFormat = NULL, ColumnHeaderLabel1 = NULL, ColumnHeaderLabel2 = NULL, ColumnHeaderLabel3 = NULL, ColumnHeaderLabel4 = NULL, ColumnHeaderLabel5 = NULL, ColumnHeaderLabel6 = NULL, ColumnHeaderLabel7 = NULL, ColumnHeaderLabel8 = NULL, ColumnHeaderLabel9 = NULL, ColumnHeaderLabel10 = NULL, IsDefault = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "DisciplineLetterTemplate", body = list(DataObject = body), searchFields = append("DisciplineLetterTemplateID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DisciplineLetterTemplate
	#'
	#' This function modifies a DisciplineLetterTemplate
	#' @param fieldNames The field values to give the modified DisciplineLetterTemplate. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified DisciplineLetterTemplate
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDisciplineLetterTemplate <- function(DisciplineLetterTemplateID, Description = NULL, Header = NULL, Body = NULL, Footer = NULL, DistrictID = NULL, SchoolYearID = NULL, DisciplineLetterTemplateIDClonedFrom = NULL, MediaIDLogo = NULL, GuardianNameFormat = NULL, StudentNameFormat = NULL, AdvisorNameFormat = NULL, SuperintendentNameFormat = NULL, ColumnHeaderLabel1 = NULL, ColumnHeaderLabel2 = NULL, ColumnHeaderLabel3 = NULL, ColumnHeaderLabel4 = NULL, ColumnHeaderLabel5 = NULL, ColumnHeaderLabel6 = NULL, ColumnHeaderLabel7 = NULL, ColumnHeaderLabel8 = NULL, ColumnHeaderLabel9 = NULL, ColumnHeaderLabel10 = NULL, IsDefault = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "DisciplineLetterTemplate", objectId = DisciplineLetterTemplateID, body = list(DataObject = body), searchFields = append("DisciplineLetterTemplateID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DisciplineLetterRunHistoryActions
	#'
	#' This function returns a dataframe or json object of DisciplineLetterRunHistoryActions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DisciplineLetterRunHistoryActions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DisciplineLetterRunHistoryActions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DisciplineLetterRunHistoryAction') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of DisciplineLetterRunHistoryActions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDisciplineLetterRunHistoryActions <- function(searchConditionsList = NULL, DisciplineLetterRunHistoryActionID = F, ActionID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DisciplineLetterRunHistoryID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "DisciplineLetterRunHistoryAction", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DisciplineLetterRunHistoryAction
	#'
	#' This function returns a dataframe or json object of a DisciplineLetterRunHistoryAction
	#' @param DisciplineLetterRunHistoryActionID The ID of the DisciplineLetterRunHistoryAction to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DisciplineLetterRunHistoryAction. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DisciplineLetterRunHistoryAction.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DisciplineLetterRunHistoryAction') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of DisciplineLetterRunHistoryAction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDisciplineLetterRunHistoryAction <- function(DisciplineLetterRunHistoryActionID, ActionID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DisciplineLetterRunHistoryID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DisciplineLetterRunHistoryActionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "DisciplineLetterRunHistoryAction", objectId = DisciplineLetterRunHistoryActionID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DisciplineLetterRunHistoryAction
	#'
	#' This function deletes a DisciplineLetterRunHistoryAction
	#' @param DisciplineLetterRunHistoryActionID The ID of the DisciplineLetterRunHistoryAction to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The DisciplineLetterRunHistoryActionID of the deleted DisciplineLetterRunHistoryAction.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDisciplineLetterRunHistoryAction <- function(DisciplineLetterRunHistoryActionID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "DisciplineLetterRunHistoryAction", objectId = DisciplineLetterRunHistoryActionID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DisciplineLetterRunHistoryAction
	#'
	#' This function creates a DisciplineLetterRunHistoryAction
	#' @param fieldNames The field values to give the created DisciplineLetterRunHistoryAction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created DisciplineLetterRunHistoryAction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDisciplineLetterRunHistoryAction <- function(ActionID = NULL, DisciplineLetterRunHistoryID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "DisciplineLetterRunHistoryAction", body = list(DataObject = body), searchFields = append("DisciplineLetterRunHistoryActionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DisciplineLetterRunHistoryAction
	#'
	#' This function modifies a DisciplineLetterRunHistoryAction
	#' @param fieldNames The field values to give the modified DisciplineLetterRunHistoryAction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified DisciplineLetterRunHistoryAction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDisciplineLetterRunHistoryAction <- function(DisciplineLetterRunHistoryActionID, ActionID = NULL, DisciplineLetterRunHistoryID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "DisciplineLetterRunHistoryAction", objectId = DisciplineLetterRunHistoryActionID, body = list(DataObject = body), searchFields = append("DisciplineLetterRunHistoryActionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DisciplineLetterTemplateEntities
	#'
	#' This function returns a dataframe or json object of DisciplineLetterTemplateEntities
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DisciplineLetterTemplateEntities. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DisciplineLetterTemplateEntities.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DisciplineLetterTemplateEntity') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of DisciplineLetterTemplateEntities
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDisciplineLetterTemplateEntities <- function(searchConditionsList = NULL, DisciplineLetterTemplateEntityID = F, DisciplineLetterTemplateID = F, EntityID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "DisciplineLetterTemplateEntity", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DisciplineLetterTemplateEntity
	#'
	#' This function returns a dataframe or json object of a DisciplineLetterTemplateEntity
	#' @param DisciplineLetterTemplateEntityID The ID of the DisciplineLetterTemplateEntity to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DisciplineLetterTemplateEntity. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DisciplineLetterTemplateEntity.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DisciplineLetterTemplateEntity') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of DisciplineLetterTemplateEntity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDisciplineLetterTemplateEntity <- function(DisciplineLetterTemplateEntityID, DisciplineLetterTemplateID = F, EntityID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DisciplineLetterTemplateEntityID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "DisciplineLetterTemplateEntity", objectId = DisciplineLetterTemplateEntityID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DisciplineLetterTemplateEntity
	#'
	#' This function deletes a DisciplineLetterTemplateEntity
	#' @param DisciplineLetterTemplateEntityID The ID of the DisciplineLetterTemplateEntity to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The DisciplineLetterTemplateEntityID of the deleted DisciplineLetterTemplateEntity.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDisciplineLetterTemplateEntity <- function(DisciplineLetterTemplateEntityID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "DisciplineLetterTemplateEntity", objectId = DisciplineLetterTemplateEntityID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DisciplineLetterTemplateEntity
	#'
	#' This function creates a DisciplineLetterTemplateEntity
	#' @param fieldNames The field values to give the created DisciplineLetterTemplateEntity. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created DisciplineLetterTemplateEntity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDisciplineLetterTemplateEntity <- function(DisciplineLetterTemplateID = NULL, EntityID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "DisciplineLetterTemplateEntity", body = list(DataObject = body), searchFields = append("DisciplineLetterTemplateEntityID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DisciplineLetterTemplateEntity
	#'
	#' This function modifies a DisciplineLetterTemplateEntity
	#' @param fieldNames The field values to give the modified DisciplineLetterTemplateEntity. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified DisciplineLetterTemplateEntity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDisciplineLetterTemplateEntity <- function(DisciplineLetterTemplateEntityID, DisciplineLetterTemplateID = NULL, EntityID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "DisciplineLetterTemplateEntity", objectId = DisciplineLetterTemplateEntityID, body = list(DataObject = body), searchFields = append("DisciplineLetterTemplateEntityID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DisciplineLetterRunHistoryOffenses
	#'
	#' This function returns a dataframe or json object of DisciplineLetterRunHistoryOffenses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DisciplineLetterRunHistoryOffenses. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DisciplineLetterRunHistoryOffenses.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DisciplineLetterRunHistoryOffense') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of DisciplineLetterRunHistoryOffenses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDisciplineLetterRunHistoryOffenses <- function(searchConditionsList = NULL, DisciplineLetterRunHistoryOffenseID = F, DisciplineLetterRunHistoryID = F, OffenseID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "DisciplineLetterRunHistoryOffense", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DisciplineLetterRunHistoryOffense
	#'
	#' This function returns a dataframe or json object of a DisciplineLetterRunHistoryOffense
	#' @param DisciplineLetterRunHistoryOffenseID The ID of the DisciplineLetterRunHistoryOffense to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DisciplineLetterRunHistoryOffense. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DisciplineLetterRunHistoryOffense.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DisciplineLetterRunHistoryOffense') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of DisciplineLetterRunHistoryOffense
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDisciplineLetterRunHistoryOffense <- function(DisciplineLetterRunHistoryOffenseID, DisciplineLetterRunHistoryID = F, OffenseID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DisciplineLetterRunHistoryOffenseID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "DisciplineLetterRunHistoryOffense", objectId = DisciplineLetterRunHistoryOffenseID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DisciplineLetterRunHistoryOffense
	#'
	#' This function deletes a DisciplineLetterRunHistoryOffense
	#' @param DisciplineLetterRunHistoryOffenseID The ID of the DisciplineLetterRunHistoryOffense to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The DisciplineLetterRunHistoryOffenseID of the deleted DisciplineLetterRunHistoryOffense.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDisciplineLetterRunHistoryOffense <- function(DisciplineLetterRunHistoryOffenseID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "DisciplineLetterRunHistoryOffense", objectId = DisciplineLetterRunHistoryOffenseID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DisciplineLetterRunHistoryOffense
	#'
	#' This function creates a DisciplineLetterRunHistoryOffense
	#' @param fieldNames The field values to give the created DisciplineLetterRunHistoryOffense. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created DisciplineLetterRunHistoryOffense
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDisciplineLetterRunHistoryOffense <- function(DisciplineLetterRunHistoryID = NULL, OffenseID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "DisciplineLetterRunHistoryOffense", body = list(DataObject = body), searchFields = append("DisciplineLetterRunHistoryOffenseID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DisciplineLetterRunHistoryOffense
	#'
	#' This function modifies a DisciplineLetterRunHistoryOffense
	#' @param fieldNames The field values to give the modified DisciplineLetterRunHistoryOffense. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified DisciplineLetterRunHistoryOffense
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDisciplineLetterRunHistoryOffense <- function(DisciplineLetterRunHistoryOffenseID, DisciplineLetterRunHistoryID = NULL, OffenseID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "DisciplineLetterRunHistoryOffense", objectId = DisciplineLetterRunHistoryOffenseID, body = list(DataObject = body), searchFields = append("DisciplineLetterRunHistoryOffenseID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List IncidentOffenseNameReportRunHistories
	#'
	#' This function returns a dataframe or json object of IncidentOffenseNameReportRunHistories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given IncidentOffenseNameReportRunHistories. Defaults to FALSE for all return fields which, for convenience, returns all fields for the IncidentOffenseNameReportRunHistories.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('IncidentOffenseNameReportRunHistory') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of IncidentOffenseNameReportRunHistories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listIncidentOffenseNameReportRunHistories <- function(searchConditionsList = NULL, IncidentOffenseNameReportRunHistoryID = F, DisciplineLetterRunHistoryID = F, IncidentOffenseNameID = F, StudentID = F, IsActive = F, AttachmentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ColumnHeaderData1 = F, ColumnHeaderData2 = F, ColumnHeaderData3 = F, ColumnHeaderData4 = F, ColumnHeaderData5 = F, ColumnHeaderData6 = F, ColumnHeaderData7 = F, ColumnHeaderData8 = F, ColumnHeaderData9 = F, ColumnHeaderData10 = F, UnboundHeader = F, UnboundBody = F, UnboundFooter = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "IncidentOffenseNameReportRunHistory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an IncidentOffenseNameReportRunHistory
	#'
	#' This function returns a dataframe or json object of an IncidentOffenseNameReportRunHistory
	#' @param IncidentOffenseNameReportRunHistoryID The ID of the IncidentOffenseNameReportRunHistory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given IncidentOffenseNameReportRunHistory. Defaults to FALSE for all return fields which, for convenience, returns all fields for the IncidentOffenseNameReportRunHistory.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('IncidentOffenseNameReportRunHistory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of IncidentOffenseNameReportRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getIncidentOffenseNameReportRunHistory <- function(IncidentOffenseNameReportRunHistoryID, DisciplineLetterRunHistoryID = F, IncidentOffenseNameID = F, StudentID = F, IsActive = F, AttachmentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ColumnHeaderData1 = F, ColumnHeaderData2 = F, ColumnHeaderData3 = F, ColumnHeaderData4 = F, ColumnHeaderData5 = F, ColumnHeaderData6 = F, ColumnHeaderData7 = F, ColumnHeaderData8 = F, ColumnHeaderData9 = F, ColumnHeaderData10 = F, UnboundHeader = F, UnboundBody = F, UnboundFooter = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "IncidentOffenseNameReportRunHistoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "IncidentOffenseNameReportRunHistory", objectId = IncidentOffenseNameReportRunHistoryID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an IncidentOffenseNameReportRunHistory
	#'
	#' This function deletes an IncidentOffenseNameReportRunHistory
	#' @param IncidentOffenseNameReportRunHistoryID The ID of the IncidentOffenseNameReportRunHistory to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The IncidentOffenseNameReportRunHistoryID of the deleted IncidentOffenseNameReportRunHistory.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteIncidentOffenseNameReportRunHistory <- function(IncidentOffenseNameReportRunHistoryID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "IncidentOffenseNameReportRunHistory", objectId = IncidentOffenseNameReportRunHistoryID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an IncidentOffenseNameReportRunHistory
	#'
	#' This function creates an IncidentOffenseNameReportRunHistory
	#' @param fieldNames The field values to give the created IncidentOffenseNameReportRunHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created IncidentOffenseNameReportRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createIncidentOffenseNameReportRunHistory <- function(DisciplineLetterRunHistoryID = NULL, IncidentOffenseNameID = NULL, StudentID = NULL, IsActive = NULL, AttachmentID = NULL, ColumnHeaderData1 = NULL, ColumnHeaderData2 = NULL, ColumnHeaderData3 = NULL, ColumnHeaderData4 = NULL, ColumnHeaderData5 = NULL, ColumnHeaderData6 = NULL, ColumnHeaderData7 = NULL, ColumnHeaderData8 = NULL, ColumnHeaderData9 = NULL, ColumnHeaderData10 = NULL, UnboundHeader = NULL, UnboundBody = NULL, UnboundFooter = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "IncidentOffenseNameReportRunHistory", body = list(DataObject = body), searchFields = append("IncidentOffenseNameReportRunHistoryID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an IncidentOffenseNameReportRunHistory
	#'
	#' This function modifies an IncidentOffenseNameReportRunHistory
	#' @param fieldNames The field values to give the modified IncidentOffenseNameReportRunHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified IncidentOffenseNameReportRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyIncidentOffenseNameReportRunHistory <- function(IncidentOffenseNameReportRunHistoryID, DisciplineLetterRunHistoryID = NULL, IncidentOffenseNameID = NULL, StudentID = NULL, IsActive = NULL, AttachmentID = NULL, ColumnHeaderData1 = NULL, ColumnHeaderData2 = NULL, ColumnHeaderData3 = NULL, ColumnHeaderData4 = NULL, ColumnHeaderData5 = NULL, ColumnHeaderData6 = NULL, ColumnHeaderData7 = NULL, ColumnHeaderData8 = NULL, ColumnHeaderData9 = NULL, ColumnHeaderData10 = NULL, UnboundHeader = NULL, UnboundBody = NULL, UnboundFooter = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "IncidentOffenseNameReportRunHistory", objectId = IncidentOffenseNameReportRunHistoryID, body = list(DataObject = body), searchFields = append("IncidentOffenseNameReportRunHistoryID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DisciplineLetterTemplateHeaderColumns
	#'
	#' This function returns a dataframe or json object of DisciplineLetterTemplateHeaderColumns
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DisciplineLetterTemplateHeaderColumns. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DisciplineLetterTemplateHeaderColumns.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DisciplineLetterTemplateHeaderColumn') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of DisciplineLetterTemplateHeaderColumns
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDisciplineLetterTemplateHeaderColumns <- function(searchConditionsList = NULL, DisciplineLetterTemplateHeaderColumnID = F, DisciplineLetterTemplateHeaderRowID = F, SortNumber = F, FieldType = F, LabelOverride = F, FreeformText = F, DisciplineLetterTemplateHeaderColumnIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ColumnNumber = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "DisciplineLetterTemplateHeaderColumn", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DisciplineLetterTemplateHeaderColumn
	#'
	#' This function returns a dataframe or json object of a DisciplineLetterTemplateHeaderColumn
	#' @param DisciplineLetterTemplateHeaderColumnID The ID of the DisciplineLetterTemplateHeaderColumn to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DisciplineLetterTemplateHeaderColumn. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DisciplineLetterTemplateHeaderColumn.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DisciplineLetterTemplateHeaderColumn') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of DisciplineLetterTemplateHeaderColumn
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDisciplineLetterTemplateHeaderColumn <- function(DisciplineLetterTemplateHeaderColumnID, DisciplineLetterTemplateHeaderRowID = F, SortNumber = F, FieldType = F, LabelOverride = F, FreeformText = F, DisciplineLetterTemplateHeaderColumnIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ColumnNumber = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DisciplineLetterTemplateHeaderColumnID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "DisciplineLetterTemplateHeaderColumn", objectId = DisciplineLetterTemplateHeaderColumnID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DisciplineLetterTemplateHeaderColumn
	#'
	#' This function deletes a DisciplineLetterTemplateHeaderColumn
	#' @param DisciplineLetterTemplateHeaderColumnID The ID of the DisciplineLetterTemplateHeaderColumn to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The DisciplineLetterTemplateHeaderColumnID of the deleted DisciplineLetterTemplateHeaderColumn.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDisciplineLetterTemplateHeaderColumn <- function(DisciplineLetterTemplateHeaderColumnID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "DisciplineLetterTemplateHeaderColumn", objectId = DisciplineLetterTemplateHeaderColumnID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DisciplineLetterTemplateHeaderColumn
	#'
	#' This function creates a DisciplineLetterTemplateHeaderColumn
	#' @param fieldNames The field values to give the created DisciplineLetterTemplateHeaderColumn. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created DisciplineLetterTemplateHeaderColumn
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDisciplineLetterTemplateHeaderColumn <- function(DisciplineLetterTemplateHeaderRowID = NULL, SortNumber = NULL, FieldType = NULL, LabelOverride = NULL, FreeformText = NULL, DisciplineLetterTemplateHeaderColumnIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "DisciplineLetterTemplateHeaderColumn", body = list(DataObject = body), searchFields = append("DisciplineLetterTemplateHeaderColumnID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DisciplineLetterTemplateHeaderColumn
	#'
	#' This function modifies a DisciplineLetterTemplateHeaderColumn
	#' @param fieldNames The field values to give the modified DisciplineLetterTemplateHeaderColumn. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified DisciplineLetterTemplateHeaderColumn
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDisciplineLetterTemplateHeaderColumn <- function(DisciplineLetterTemplateHeaderColumnID, DisciplineLetterTemplateHeaderRowID = NULL, SortNumber = NULL, FieldType = NULL, LabelOverride = NULL, FreeformText = NULL, DisciplineLetterTemplateHeaderColumnIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "DisciplineLetterTemplateHeaderColumn", objectId = DisciplineLetterTemplateHeaderColumnID, body = list(DataObject = body), searchFields = append("DisciplineLetterTemplateHeaderColumnID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DisciplineLetterRunHistories
	#'
	#' This function returns a dataframe or json object of DisciplineLetterRunHistories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DisciplineLetterRunHistories. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DisciplineLetterRunHistories.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DisciplineLetterRunHistory') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of DisciplineLetterRunHistories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDisciplineLetterRunHistories <- function(searchConditionsList = NULL, DisciplineLetterRunHistoryID = F, DisciplineLetterTemplateID = F, EntityID = F, RunDescription = F, ParameterData = F, ParameterDescription = F, Date = F, FilterType = F, IncidentType = F, GracePeriod = F, IsActive = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartDate = F, EndDate = F, ReportRunInfoID = F, SchoolYearID = F, FiscalYearID = F, CachedFiscalYear = F, SectionID = F, EntityIDList = F, SchoolYearNumericYearOrCurrent = F, CachedSchoolYear = F, CachedEntity = F, IsReadOnlyHistoricalRecord = F, PostToFASA = F, AttachmentDisplayName = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "DisciplineLetterRunHistory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DisciplineLetterRunHistory
	#'
	#' This function returns a dataframe or json object of a DisciplineLetterRunHistory
	#' @param DisciplineLetterRunHistoryID The ID of the DisciplineLetterRunHistory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DisciplineLetterRunHistory. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DisciplineLetterRunHistory.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DisciplineLetterRunHistory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of DisciplineLetterRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDisciplineLetterRunHistory <- function(DisciplineLetterRunHistoryID, DisciplineLetterTemplateID = F, EntityID = F, RunDescription = F, ParameterData = F, ParameterDescription = F, Date = F, FilterType = F, IncidentType = F, GracePeriod = F, IsActive = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartDate = F, EndDate = F, ReportRunInfoID = F, SchoolYearID = F, FiscalYearID = F, CachedFiscalYear = F, SectionID = F, EntityIDList = F, SchoolYearNumericYearOrCurrent = F, CachedSchoolYear = F, CachedEntity = F, IsReadOnlyHistoricalRecord = F, PostToFASA = F, AttachmentDisplayName = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DisciplineLetterRunHistoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "DisciplineLetterRunHistory", objectId = DisciplineLetterRunHistoryID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DisciplineLetterRunHistory
	#'
	#' This function deletes a DisciplineLetterRunHistory
	#' @param DisciplineLetterRunHistoryID The ID of the DisciplineLetterRunHistory to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The DisciplineLetterRunHistoryID of the deleted DisciplineLetterRunHistory.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDisciplineLetterRunHistory <- function(DisciplineLetterRunHistoryID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "DisciplineLetterRunHistory", objectId = DisciplineLetterRunHistoryID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DisciplineLetterRunHistory
	#'
	#' This function creates a DisciplineLetterRunHistory
	#' @param fieldNames The field values to give the created DisciplineLetterRunHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created DisciplineLetterRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDisciplineLetterRunHistory <- function(DisciplineLetterTemplateID = NULL, EntityID = NULL, RunDescription = NULL, Date = NULL, FilterType = NULL, IncidentType = NULL, GracePeriod = NULL, IsActive = NULL, StartDate = NULL, EndDate = NULL, ReportRunInfoID = NULL, PostToFASA = NULL, AttachmentDisplayName = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "DisciplineLetterRunHistory", body = list(DataObject = body), searchFields = append("DisciplineLetterRunHistoryID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DisciplineLetterRunHistory
	#'
	#' This function modifies a DisciplineLetterRunHistory
	#' @param fieldNames The field values to give the modified DisciplineLetterRunHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified DisciplineLetterRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDisciplineLetterRunHistory <- function(DisciplineLetterRunHistoryID, DisciplineLetterTemplateID = NULL, EntityID = NULL, RunDescription = NULL, Date = NULL, FilterType = NULL, IncidentType = NULL, GracePeriod = NULL, IsActive = NULL, StartDate = NULL, EndDate = NULL, ReportRunInfoID = NULL, PostToFASA = NULL, AttachmentDisplayName = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "DisciplineLetterRunHistory", objectId = DisciplineLetterRunHistoryID, body = list(DataObject = body), searchFields = append("DisciplineLetterRunHistoryID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DisciplineLetterTemplateHeaderRows
	#'
	#' This function returns a dataframe or json object of DisciplineLetterTemplateHeaderRows
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DisciplineLetterTemplateHeaderRows. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DisciplineLetterTemplateHeaderRows.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DisciplineLetterTemplateHeaderRow') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of DisciplineLetterTemplateHeaderRows
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDisciplineLetterTemplateHeaderRows <- function(searchConditionsList = NULL, DisciplineLetterTemplateHeaderRowID = F, DisciplineLetterTemplateID = F, SortNumber = F, ColumnCount = F, DisciplineLetterTemplateHeaderRowIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "DisciplineLetterTemplateHeaderRow", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DisciplineLetterTemplateHeaderRow
	#'
	#' This function returns a dataframe or json object of a DisciplineLetterTemplateHeaderRow
	#' @param DisciplineLetterTemplateHeaderRowID The ID of the DisciplineLetterTemplateHeaderRow to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DisciplineLetterTemplateHeaderRow. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DisciplineLetterTemplateHeaderRow.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DisciplineLetterTemplateHeaderRow') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of DisciplineLetterTemplateHeaderRow
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDisciplineLetterTemplateHeaderRow <- function(DisciplineLetterTemplateHeaderRowID, DisciplineLetterTemplateID = F, SortNumber = F, ColumnCount = F, DisciplineLetterTemplateHeaderRowIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DisciplineLetterTemplateHeaderRowID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "DisciplineLetterTemplateHeaderRow", objectId = DisciplineLetterTemplateHeaderRowID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DisciplineLetterTemplateHeaderRow
	#'
	#' This function deletes a DisciplineLetterTemplateHeaderRow
	#' @param DisciplineLetterTemplateHeaderRowID The ID of the DisciplineLetterTemplateHeaderRow to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The DisciplineLetterTemplateHeaderRowID of the deleted DisciplineLetterTemplateHeaderRow.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDisciplineLetterTemplateHeaderRow <- function(DisciplineLetterTemplateHeaderRowID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "DisciplineLetterTemplateHeaderRow", objectId = DisciplineLetterTemplateHeaderRowID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DisciplineLetterTemplateHeaderRow
	#'
	#' This function creates a DisciplineLetterTemplateHeaderRow
	#' @param fieldNames The field values to give the created DisciplineLetterTemplateHeaderRow. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created DisciplineLetterTemplateHeaderRow
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDisciplineLetterTemplateHeaderRow <- function(DisciplineLetterTemplateID = NULL, SortNumber = NULL, ColumnCount = NULL, DisciplineLetterTemplateHeaderRowIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "DisciplineLetterTemplateHeaderRow", body = list(DataObject = body), searchFields = append("DisciplineLetterTemplateHeaderRowID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DisciplineLetterTemplateHeaderRow
	#'
	#' This function modifies a DisciplineLetterTemplateHeaderRow
	#' @param fieldNames The field values to give the modified DisciplineLetterTemplateHeaderRow. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified DisciplineLetterTemplateHeaderRow
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDisciplineLetterTemplateHeaderRow <- function(DisciplineLetterTemplateHeaderRowID, DisciplineLetterTemplateID = NULL, SortNumber = NULL, ColumnCount = NULL, DisciplineLetterTemplateHeaderRowIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "DisciplineLetterTemplateHeaderRow", objectId = DisciplineLetterTemplateHeaderRowID, body = list(DataObject = body), searchFields = append("DisciplineLetterTemplateHeaderRowID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempIncidentOffenseNameReportRunHistoryRecords
	#'
	#' This function returns a dataframe or json object of TempIncidentOffenseNameReportRunHistoryRecords
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentOffenseNameReportRunHistoryRecords. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentOffenseNameReportRunHistoryRecords.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentOffenseNameReportRunHistoryRecord') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of TempIncidentOffenseNameReportRunHistoryRecords
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempIncidentOffenseNameReportRunHistoryRecords <- function(searchConditionsList = NULL, TempIncidentOffenseNameReportRunHistoryRecordID = F, StudentID = F, StudentName = F, IncidentOffenseNameID = F, DateTime = F, Offense = F, Incident = F, IncidentNumber = F, ColumnHeader1 = F, ColumnHeader2 = F, ColumnHeader3 = F, ColumnHeader4 = F, ColumnHeader5 = F, ColumnHeader6 = F, ColumnHeader7 = F, ColumnHeader8 = F, ColumnHeader9 = F, ColumnHeader10 = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, UnboundHeader = F, UnboundBody = F, UnboundFooter = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffenseNameReportRunHistoryRecord", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempIncidentOffenseNameReportRunHistoryRecord
	#'
	#' This function returns a dataframe or json object of a TempIncidentOffenseNameReportRunHistoryRecord
	#' @param TempIncidentOffenseNameReportRunHistoryRecordID The ID of the TempIncidentOffenseNameReportRunHistoryRecord to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentOffenseNameReportRunHistoryRecord. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentOffenseNameReportRunHistoryRecord.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentOffenseNameReportRunHistoryRecord') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of TempIncidentOffenseNameReportRunHistoryRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempIncidentOffenseNameReportRunHistoryRecord <- function(TempIncidentOffenseNameReportRunHistoryRecordID, StudentID = F, StudentName = F, IncidentOffenseNameID = F, DateTime = F, Offense = F, Incident = F, IncidentNumber = F, ColumnHeader1 = F, ColumnHeader2 = F, ColumnHeader3 = F, ColumnHeader4 = F, ColumnHeader5 = F, ColumnHeader6 = F, ColumnHeader7 = F, ColumnHeader8 = F, ColumnHeader9 = F, ColumnHeader10 = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, UnboundHeader = F, UnboundBody = F, UnboundFooter = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempIncidentOffenseNameReportRunHistoryRecordID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameReportRunHistoryRecord", objectId = TempIncidentOffenseNameReportRunHistoryRecordID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempIncidentOffenseNameReportRunHistoryRecord
	#'
	#' This function deletes a TempIncidentOffenseNameReportRunHistoryRecord
	#' @param TempIncidentOffenseNameReportRunHistoryRecordID The ID of the TempIncidentOffenseNameReportRunHistoryRecord to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The TempIncidentOffenseNameReportRunHistoryRecordID of the deleted TempIncidentOffenseNameReportRunHistoryRecord.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempIncidentOffenseNameReportRunHistoryRecord <- function(TempIncidentOffenseNameReportRunHistoryRecordID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameReportRunHistoryRecord", objectId = TempIncidentOffenseNameReportRunHistoryRecordID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempIncidentOffenseNameReportRunHistoryRecord
	#'
	#' This function creates a TempIncidentOffenseNameReportRunHistoryRecord
	#' @param fieldNames The field values to give the created TempIncidentOffenseNameReportRunHistoryRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created TempIncidentOffenseNameReportRunHistoryRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempIncidentOffenseNameReportRunHistoryRecord <- function(StudentID = NULL, StudentName = NULL, IncidentOffenseNameID = NULL, DateTime = NULL, Offense = NULL, Incident = NULL, IncidentNumber = NULL, ColumnHeader1 = NULL, ColumnHeader2 = NULL, ColumnHeader3 = NULL, ColumnHeader4 = NULL, ColumnHeader5 = NULL, ColumnHeader6 = NULL, ColumnHeader7 = NULL, ColumnHeader8 = NULL, ColumnHeader9 = NULL, ColumnHeader10 = NULL, UnboundHeader = NULL, UnboundBody = NULL, UnboundFooter = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameReportRunHistoryRecord", body = list(DataObject = body), searchFields = append("TempIncidentOffenseNameReportRunHistoryRecordID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempIncidentOffenseNameReportRunHistoryRecord
	#'
	#' This function modifies a TempIncidentOffenseNameReportRunHistoryRecord
	#' @param fieldNames The field values to give the modified TempIncidentOffenseNameReportRunHistoryRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified TempIncidentOffenseNameReportRunHistoryRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempIncidentOffenseNameReportRunHistoryRecord <- function(TempIncidentOffenseNameReportRunHistoryRecordID, StudentID = NULL, StudentName = NULL, IncidentOffenseNameID = NULL, DateTime = NULL, Offense = NULL, Incident = NULL, IncidentNumber = NULL, ColumnHeader1 = NULL, ColumnHeader2 = NULL, ColumnHeader3 = NULL, ColumnHeader4 = NULL, ColumnHeader5 = NULL, ColumnHeader6 = NULL, ColumnHeader7 = NULL, ColumnHeader8 = NULL, ColumnHeader9 = NULL, ColumnHeader10 = NULL, UnboundHeader = NULL, UnboundBody = NULL, UnboundFooter = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameReportRunHistoryRecord", objectId = TempIncidentOffenseNameReportRunHistoryRecordID, body = list(DataObject = body), searchFields = append("TempIncidentOffenseNameReportRunHistoryRecordID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List WhiteListFieldPaths
	#'
	#' This function returns a dataframe or json object of WhiteListFieldPaths
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given WhiteListFieldPaths. Defaults to FALSE for all return fields which, for convenience, returns all fields for the WhiteListFieldPaths.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('WhiteListFieldPath') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of WhiteListFieldPaths
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listWhiteListFieldPaths <- function(searchConditionsList = NULL, WhiteListFieldPathID = F, FriendlyName = F, Description = F, FieldPath = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, UsedForType = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "WhiteListFieldPath", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a WhiteListFieldPath
	#'
	#' This function returns a dataframe or json object of a WhiteListFieldPath
	#' @param WhiteListFieldPathID The ID of the WhiteListFieldPath to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given WhiteListFieldPath. Defaults to FALSE for all return fields which, for convenience, returns all fields for the WhiteListFieldPath.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('WhiteListFieldPath') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of WhiteListFieldPath
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getWhiteListFieldPath <- function(WhiteListFieldPathID, FriendlyName = F, Description = F, FieldPath = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, UsedForType = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "WhiteListFieldPathID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "WhiteListFieldPath", objectId = WhiteListFieldPathID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a WhiteListFieldPath
	#'
	#' This function deletes a WhiteListFieldPath
	#' @param WhiteListFieldPathID The ID of the WhiteListFieldPath to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The WhiteListFieldPathID of the deleted WhiteListFieldPath.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteWhiteListFieldPath <- function(WhiteListFieldPathID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "WhiteListFieldPath", objectId = WhiteListFieldPathID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a WhiteListFieldPath
	#'
	#' This function creates a WhiteListFieldPath
	#' @param fieldNames The field values to give the created WhiteListFieldPath. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created WhiteListFieldPath
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createWhiteListFieldPath <- function(FriendlyName = NULL, Description = NULL, UsedForType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "WhiteListFieldPath", body = list(DataObject = body), searchFields = append("WhiteListFieldPathID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a WhiteListFieldPath
	#'
	#' This function modifies a WhiteListFieldPath
	#' @param fieldNames The field values to give the modified WhiteListFieldPath. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified WhiteListFieldPath
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyWhiteListFieldPath <- function(WhiteListFieldPathID, FriendlyName = NULL, Description = NULL, UsedForType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "WhiteListFieldPath", objectId = WhiteListFieldPathID, body = list(DataObject = body), searchFields = append("WhiteListFieldPathID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DisciplineConfigEntityYears
	#'
	#' This function returns a dataframe or json object of DisciplineConfigEntityYears
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DisciplineConfigEntityYears. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DisciplineConfigEntityYears.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DisciplineConfigEntityYear') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of DisciplineConfigEntityYears
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDisciplineConfigEntityYears <- function(searchConditionsList = NULL, ConfigEntityYearID = F, EntityID = F, SchoolYearID = F, ConfigEntityYearIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "ConfigEntityYear", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DisciplineConfigEntityYear
	#'
	#' This function returns a dataframe or json object of a DisciplineConfigEntityYear
	#' @param DisciplineConfigEntityYearID The ID of the DisciplineConfigEntityYear to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DisciplineConfigEntityYear. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DisciplineConfigEntityYear.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DisciplineConfigEntityYear') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of DisciplineConfigEntityYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDisciplineConfigEntityYear <- function(DisciplineConfigEntityYearID, ConfigEntityYearID = F, EntityID = F, SchoolYearID = F, ConfigEntityYearIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DisciplineConfigEntityYearID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "ConfigEntityYear", objectId = DisciplineConfigEntityYearID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DisciplineConfigEntityYear
	#'
	#' This function deletes a DisciplineConfigEntityYear
	#' @param DisciplineConfigEntityYearID The ID of the DisciplineConfigEntityYear to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The DisciplineConfigEntityYearID of the deleted DisciplineConfigEntityYear.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDisciplineConfigEntityYear <- function(DisciplineConfigEntityYearID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "ConfigEntityYear", objectId = DisciplineConfigEntityYearID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DisciplineConfigEntityYear
	#'
	#' This function creates a DisciplineConfigEntityYear
	#' @param fieldNames The field values to give the created DisciplineConfigEntityYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created DisciplineConfigEntityYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDisciplineConfigEntityYear <- function(EntityID = NULL, SchoolYearID = NULL, ConfigEntityYearIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "ConfigEntityYear", body = list(DataObject = body), searchFields = append("ConfigEntityYearID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DisciplineConfigEntityYear
	#'
	#' This function modifies a DisciplineConfigEntityYear
	#' @param fieldNames The field values to give the modified DisciplineConfigEntityYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified DisciplineConfigEntityYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDisciplineConfigEntityYear <- function(ConfigEntityYearID, EntityID = NULL, SchoolYearID = NULL, ConfigEntityYearIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "ConfigEntityYear", objectId = ConfigEntityYearID, body = list(DataObject = body), searchFields = append("ConfigEntityYearID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempIncidentOffenseNames
	#'
	#' This function returns a dataframe or json object of TempIncidentOffenseNames
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentOffenseNames. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentOffenseNames.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentOffenseName') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of TempIncidentOffenseNames
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempIncidentOffenseNames <- function(searchConditionsList = NULL, TempIncidentOffenseNameID = F, TempIncidentInvolvedPersonID = F, TempIncidentOffenseID = F, OffenseID = F, ExistingIncidentOffenseNameID = F, InvolvementType = F, PerceivedMotivationID = F, OffenseLevelID = F, FullName = F, StudentNumber = F, OffenseCodeDescription = F, IsPrimaryOffense = F, PerceivedMotivationCodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, InternalComment = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffenseName", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempIncidentOffenseName
	#'
	#' This function returns a dataframe or json object of a TempIncidentOffenseName
	#' @param TempIncidentOffenseNameID The ID of the TempIncidentOffenseName to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentOffenseName. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentOffenseName.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentOffenseName') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of TempIncidentOffenseName
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempIncidentOffenseName <- function(TempIncidentOffenseNameID, TempIncidentInvolvedPersonID = F, TempIncidentOffenseID = F, OffenseID = F, ExistingIncidentOffenseNameID = F, InvolvementType = F, PerceivedMotivationID = F, OffenseLevelID = F, FullName = F, StudentNumber = F, OffenseCodeDescription = F, IsPrimaryOffense = F, PerceivedMotivationCodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, InternalComment = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempIncidentOffenseNameID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "TempIncidentOffenseName", objectId = TempIncidentOffenseNameID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempIncidentOffenseName
	#'
	#' This function deletes a TempIncidentOffenseName
	#' @param TempIncidentOffenseNameID The ID of the TempIncidentOffenseName to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The TempIncidentOffenseNameID of the deleted TempIncidentOffenseName.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempIncidentOffenseName <- function(TempIncidentOffenseNameID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "TempIncidentOffenseName", objectId = TempIncidentOffenseNameID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempIncidentOffenseName
	#'
	#' This function creates a TempIncidentOffenseName
	#' @param fieldNames The field values to give the created TempIncidentOffenseName. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created TempIncidentOffenseName
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempIncidentOffenseName <- function(TempIncidentInvolvedPersonID = NULL, TempIncidentOffenseID = NULL, OffenseID = NULL, ExistingIncidentOffenseNameID = NULL, InvolvementType = NULL, PerceivedMotivationID = NULL, OffenseLevelID = NULL, FullName = NULL, StudentNumber = NULL, OffenseCodeDescription = NULL, IsPrimaryOffense = NULL, PerceivedMotivationCodeDescription = NULL, InternalComment = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "TempIncidentOffenseName", body = list(DataObject = body), searchFields = append("TempIncidentOffenseNameID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempIncidentOffenseName
	#'
	#' This function modifies a TempIncidentOffenseName
	#' @param fieldNames The field values to give the modified TempIncidentOffenseName. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified TempIncidentOffenseName
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempIncidentOffenseName <- function(TempIncidentOffenseNameID, TempIncidentInvolvedPersonID = NULL, TempIncidentOffenseID = NULL, OffenseID = NULL, ExistingIncidentOffenseNameID = NULL, InvolvementType = NULL, PerceivedMotivationID = NULL, OffenseLevelID = NULL, FullName = NULL, StudentNumber = NULL, OffenseCodeDescription = NULL, IsPrimaryOffense = NULL, PerceivedMotivationCodeDescription = NULL, InternalComment = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "TempIncidentOffenseName", objectId = TempIncidentOffenseNameID, body = list(DataObject = body), searchFields = append("TempIncidentOffenseNameID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempIncidentOffenses
	#'
	#' This function returns a dataframe or json object of TempIncidentOffenses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentOffenses. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentOffenses.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentOffense') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of TempIncidentOffenses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempIncidentOffenses <- function(searchConditionsList = NULL, TempIncidentOffenseID = F, ExistingIncidentID = F, OffenseID = F, IsPrimaryOffense = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffense", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempIncidentOffense
	#'
	#' This function returns a dataframe or json object of a TempIncidentOffense
	#' @param TempIncidentOffenseID The ID of the TempIncidentOffense to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentOffense. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentOffense.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentOffense') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of TempIncidentOffense
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempIncidentOffense <- function(TempIncidentOffenseID, ExistingIncidentID = F, OffenseID = F, IsPrimaryOffense = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempIncidentOffenseID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "TempIncidentOffense", objectId = TempIncidentOffenseID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempIncidentOffense
	#'
	#' This function deletes a TempIncidentOffense
	#' @param TempIncidentOffenseID The ID of the TempIncidentOffense to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The TempIncidentOffenseID of the deleted TempIncidentOffense.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempIncidentOffense <- function(TempIncidentOffenseID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "TempIncidentOffense", objectId = TempIncidentOffenseID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempIncidentOffense
	#'
	#' This function creates a TempIncidentOffense
	#' @param fieldNames The field values to give the created TempIncidentOffense. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created TempIncidentOffense
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempIncidentOffense <- function(ExistingIncidentID = NULL, OffenseID = NULL, IsPrimaryOffense = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "TempIncidentOffense", body = list(DataObject = body), searchFields = append("TempIncidentOffenseID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempIncidentOffense
	#'
	#' This function modifies a TempIncidentOffense
	#' @param fieldNames The field values to give the modified TempIncidentOffense. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified TempIncidentOffense
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempIncidentOffense <- function(TempIncidentOffenseID, ExistingIncidentID = NULL, OffenseID = NULL, IsPrimaryOffense = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "TempIncidentOffense", objectId = TempIncidentOffenseID, body = list(DataObject = body), searchFields = append("TempIncidentOffenseID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempIncidentOffenseNameParentalInvolvementPAS
	#'
	#' This function returns a dataframe or json object of TempIncidentOffenseNameParentalInvolvementPAS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentOffenseNameParentalInvolvementPAS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentOffenseNameParentalInvolvementPAS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentOffenseNameParentalInvolvementPA') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of TempIncidentOffenseNameParentalInvolvementPAS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempIncidentOffenseNameParentalInvolvementPAS <- function(searchConditionsList = NULL, TempIncidentOffenseNameParentalInvolvementPAID = F, IncidentOffenseNameParentalInvolvementPAID = F, TempIncidentOffenseNameID = F, StateParentalInvolvementPAID = F, Comment = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffenseNameParentalInvolvementPA", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempIncidentOffenseNameParentalInvolvementPA
	#'
	#' This function returns a dataframe or json object of a TempIncidentOffenseNameParentalInvolvementPA
	#' @param TempIncidentOffenseNameParentalInvolvementPAID The ID of the TempIncidentOffenseNameParentalInvolvementPA to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentOffenseNameParentalInvolvementPA. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentOffenseNameParentalInvolvementPA.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentOffenseNameParentalInvolvementPA') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of TempIncidentOffenseNameParentalInvolvementPA
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempIncidentOffenseNameParentalInvolvementPA <- function(TempIncidentOffenseNameParentalInvolvementPAID, IncidentOffenseNameParentalInvolvementPAID = F, TempIncidentOffenseNameID = F, StateParentalInvolvementPAID = F, Comment = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempIncidentOffenseNameParentalInvolvementPAID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameParentalInvolvementPA", objectId = TempIncidentOffenseNameParentalInvolvementPAID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempIncidentOffenseNameParentalInvolvementPA
	#'
	#' This function deletes a TempIncidentOffenseNameParentalInvolvementPA
	#' @param TempIncidentOffenseNameParentalInvolvementPAID The ID of the TempIncidentOffenseNameParentalInvolvementPA to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The TempIncidentOffenseNameParentalInvolvementPAID of the deleted TempIncidentOffenseNameParentalInvolvementPA.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempIncidentOffenseNameParentalInvolvementPA <- function(TempIncidentOffenseNameParentalInvolvementPAID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameParentalInvolvementPA", objectId = TempIncidentOffenseNameParentalInvolvementPAID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempIncidentOffenseNameParentalInvolvementPA
	#'
	#' This function creates a TempIncidentOffenseNameParentalInvolvementPA
	#' @param fieldNames The field values to give the created TempIncidentOffenseNameParentalInvolvementPA. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created TempIncidentOffenseNameParentalInvolvementPA
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempIncidentOffenseNameParentalInvolvementPA <- function(TempIncidentOffenseNameID = NULL, StateParentalInvolvementPAID = NULL, Comment = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameParentalInvolvementPA", body = list(DataObject = body), searchFields = append("TempIncidentOffenseNameParentalInvolvementPAID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempIncidentOffenseNameParentalInvolvementPA
	#'
	#' This function modifies a TempIncidentOffenseNameParentalInvolvementPA
	#' @param fieldNames The field values to give the modified TempIncidentOffenseNameParentalInvolvementPA. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified TempIncidentOffenseNameParentalInvolvementPA
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempIncidentOffenseNameParentalInvolvementPA <- function(TempIncidentOffenseNameParentalInvolvementPAID, TempIncidentOffenseNameID = NULL, StateParentalInvolvementPAID = NULL, Comment = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameParentalInvolvementPA", objectId = TempIncidentOffenseNameParentalInvolvementPAID, body = list(DataObject = body), searchFields = append("TempIncidentOffenseNameParentalInvolvementPAID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempIncidentOffenseNameDrugs
	#'
	#' This function returns a dataframe or json object of TempIncidentOffenseNameDrugs
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentOffenseNameDrugs. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentOffenseNameDrugs.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentOffenseNameDrug') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of TempIncidentOffenseNameDrugs
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempIncidentOffenseNameDrugs <- function(searchConditionsList = NULL, TempIncidentOffenseNameDrugID = F, IncidentOffenseNameDrugID = F, TempIncidentOffenseNameID = F, DrugID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffenseNameDrug", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempIncidentOffenseNameDrug
	#'
	#' This function returns a dataframe or json object of a TempIncidentOffenseNameDrug
	#' @param TempIncidentOffenseNameDrugID The ID of the TempIncidentOffenseNameDrug to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentOffenseNameDrug. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentOffenseNameDrug.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentOffenseNameDrug') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of TempIncidentOffenseNameDrug
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempIncidentOffenseNameDrug <- function(TempIncidentOffenseNameDrugID, IncidentOffenseNameDrugID = F, TempIncidentOffenseNameID = F, DrugID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempIncidentOffenseNameDrugID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameDrug", objectId = TempIncidentOffenseNameDrugID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempIncidentOffenseNameDrug
	#'
	#' This function deletes a TempIncidentOffenseNameDrug
	#' @param TempIncidentOffenseNameDrugID The ID of the TempIncidentOffenseNameDrug to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The TempIncidentOffenseNameDrugID of the deleted TempIncidentOffenseNameDrug.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempIncidentOffenseNameDrug <- function(TempIncidentOffenseNameDrugID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameDrug", objectId = TempIncidentOffenseNameDrugID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempIncidentOffenseNameDrug
	#'
	#' This function creates a TempIncidentOffenseNameDrug
	#' @param fieldNames The field values to give the created TempIncidentOffenseNameDrug. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created TempIncidentOffenseNameDrug
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempIncidentOffenseNameDrug <- function(TempIncidentOffenseNameID = NULL, DrugID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameDrug", body = list(DataObject = body), searchFields = append("TempIncidentOffenseNameDrugID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempIncidentOffenseNameDrug
	#'
	#' This function modifies a TempIncidentOffenseNameDrug
	#' @param fieldNames The field values to give the modified TempIncidentOffenseNameDrug. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified TempIncidentOffenseNameDrug
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempIncidentOffenseNameDrug <- function(TempIncidentOffenseNameDrugID, TempIncidentOffenseNameID = NULL, DrugID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameDrug", objectId = TempIncidentOffenseNameDrugID, body = list(DataObject = body), searchFields = append("TempIncidentOffenseNameDrugID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempIncidentOffenseNameWeaponMNS
	#'
	#' This function returns a dataframe or json object of TempIncidentOffenseNameWeaponMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentOffenseNameWeaponMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentOffenseNameWeaponMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentOffenseNameWeaponMN') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of TempIncidentOffenseNameWeaponMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempIncidentOffenseNameWeaponMNS <- function(searchConditionsList = NULL, MeetsFederalStatuteOfDangerousWeapon = F, MeetsStateStatuteOfDangerousWeapon = F, GunWasLoaded = F, GunWasInCase = F, GunFoundInTrunk = F, TempIncidentOffenseNameWeaponID = F, IncidentOffenseNameWeaponID = F, TempIncidentOffenseNameID = F, WeaponID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, WeaponCount = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffenseNameWeaponMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempIncidentOffenseNameWeaponMN
	#'
	#' This function returns a dataframe or json object of a TempIncidentOffenseNameWeaponMN
	#' @param TempIncidentOffenseNameWeaponMNID The ID of the TempIncidentOffenseNameWeaponMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentOffenseNameWeaponMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentOffenseNameWeaponMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentOffenseNameWeaponMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of TempIncidentOffenseNameWeaponMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempIncidentOffenseNameWeaponMN <- function(TempIncidentOffenseNameWeaponMNID, MeetsFederalStatuteOfDangerousWeapon = F, MeetsStateStatuteOfDangerousWeapon = F, GunWasLoaded = F, GunWasInCase = F, GunFoundInTrunk = F, TempIncidentOffenseNameWeaponID = F, IncidentOffenseNameWeaponID = F, TempIncidentOffenseNameID = F, WeaponID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, WeaponCount = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempIncidentOffenseNameWeaponMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameWeaponMN", objectId = TempIncidentOffenseNameWeaponMNID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempIncidentOffenseNameWeaponMN
	#'
	#' This function deletes a TempIncidentOffenseNameWeaponMN
	#' @param TempIncidentOffenseNameWeaponMNID The ID of the TempIncidentOffenseNameWeaponMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The TempIncidentOffenseNameWeaponMNID of the deleted TempIncidentOffenseNameWeaponMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempIncidentOffenseNameWeaponMN <- function(TempIncidentOffenseNameWeaponMNID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameWeaponMN", objectId = TempIncidentOffenseNameWeaponMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempIncidentOffenseNameWeaponMN
	#'
	#' This function creates a TempIncidentOffenseNameWeaponMN
	#' @param fieldNames The field values to give the created TempIncidentOffenseNameWeaponMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created TempIncidentOffenseNameWeaponMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempIncidentOffenseNameWeaponMN <- function(MeetsFederalStatuteOfDangerousWeapon = NULL, MeetsStateStatuteOfDangerousWeapon = NULL, GunWasLoaded = NULL, GunWasInCase = NULL, GunFoundInTrunk = NULL, TempIncidentOffenseNameID = NULL, WeaponID = NULL, WeaponCount = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameWeaponMN", body = list(DataObject = body), searchFields = append("TempIncidentOffenseNameWeaponMNID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempIncidentOffenseNameWeaponMN
	#'
	#' This function modifies a TempIncidentOffenseNameWeaponMN
	#' @param fieldNames The field values to give the modified TempIncidentOffenseNameWeaponMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified TempIncidentOffenseNameWeaponMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempIncidentOffenseNameWeaponMN <- function(TempIncidentOffenseNameWeaponMNID, MeetsFederalStatuteOfDangerousWeapon = NULL, MeetsStateStatuteOfDangerousWeapon = NULL, GunWasLoaded = NULL, GunWasInCase = NULL, GunFoundInTrunk = NULL, TempIncidentOffenseNameID = NULL, WeaponID = NULL, WeaponCount = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameWeaponMN", objectId = TempIncidentOffenseNameWeaponMNID, body = list(DataObject = body), searchFields = append("TempIncidentOffenseNameWeaponMNID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempIncidentOffenseNameWeapons
	#'
	#' This function returns a dataframe or json object of TempIncidentOffenseNameWeapons
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentOffenseNameWeapons. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentOffenseNameWeapons.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentOffenseNameWeapon') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of TempIncidentOffenseNameWeapons
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempIncidentOffenseNameWeapons <- function(searchConditionsList = NULL, TempIncidentOffenseNameWeaponID = F, IncidentOffenseNameWeaponID = F, TempIncidentOffenseNameID = F, WeaponID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, WeaponCount = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffenseNameWeapon", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempIncidentOffenseNameWeapon
	#'
	#' This function returns a dataframe or json object of a TempIncidentOffenseNameWeapon
	#' @param TempIncidentOffenseNameWeaponID The ID of the TempIncidentOffenseNameWeapon to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentOffenseNameWeapon. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentOffenseNameWeapon.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentOffenseNameWeapon') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of TempIncidentOffenseNameWeapon
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempIncidentOffenseNameWeapon <- function(TempIncidentOffenseNameWeaponID, IncidentOffenseNameWeaponID = F, TempIncidentOffenseNameID = F, WeaponID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, WeaponCount = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempIncidentOffenseNameWeaponID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameWeapon", objectId = TempIncidentOffenseNameWeaponID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempIncidentOffenseNameWeapon
	#'
	#' This function deletes a TempIncidentOffenseNameWeapon
	#' @param TempIncidentOffenseNameWeaponID The ID of the TempIncidentOffenseNameWeapon to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The TempIncidentOffenseNameWeaponID of the deleted TempIncidentOffenseNameWeapon.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempIncidentOffenseNameWeapon <- function(TempIncidentOffenseNameWeaponID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameWeapon", objectId = TempIncidentOffenseNameWeaponID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempIncidentOffenseNameWeapon
	#'
	#' This function creates a TempIncidentOffenseNameWeapon
	#' @param fieldNames The field values to give the created TempIncidentOffenseNameWeapon. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created TempIncidentOffenseNameWeapon
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempIncidentOffenseNameWeapon <- function(TempIncidentOffenseNameID = NULL, WeaponID = NULL, WeaponCount = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameWeapon", body = list(DataObject = body), searchFields = append("TempIncidentOffenseNameWeaponID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempIncidentOffenseNameWeapon
	#'
	#' This function modifies a TempIncidentOffenseNameWeapon
	#' @param fieldNames The field values to give the modified TempIncidentOffenseNameWeapon. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified TempIncidentOffenseNameWeapon
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempIncidentOffenseNameWeapon <- function(TempIncidentOffenseNameWeaponID, TempIncidentOffenseNameID = NULL, WeaponID = NULL, WeaponCount = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameWeapon", objectId = TempIncidentOffenseNameWeaponID, body = list(DataObject = body), searchFields = append("TempIncidentOffenseNameWeaponID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempIncidentOffenseNameActionWIS
	#'
	#' This function returns a dataframe or json object of TempIncidentOffenseNameActionWIS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentOffenseNameActionWIS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentOffenseNameActionWIS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentOffenseNameActionWI') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of TempIncidentOffenseNameActionWIS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempIncidentOffenseNameActionWIS <- function(searchConditionsList = NULL, StateModifiedTermWIID = F, HasEarlyReinstatementCondition = F, BehaviorDetailedDescription = F, IAESRemovalType = F, CausedSeriousBodilyInjury = F, TempIncidentOffenseNameActionID = F, EntityID = F, FullName = F, StudentNumber = F, InvolvementType = F, OffenseCodeDescription = F, IsPrimaryOffense = F, ActionTypeID = F, ActionTypeCode = F, ActionID = F, ActionCodeDescription = F, Status = F, OrderedDate = F, DurationType = F, DurationToServe = F, StaffIDAuthorizedBy = F, StaffIDAuthorizedByName = F, StaffIDFollowUpOfficer = F, LocationID = F, BuildingID = F, RoomID = F, PerceivedMotivationCodeDescription = F, Comment = F, TempIncidentOffenseNameID = F, IsGuardianNotified = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartTime = F, InternalComment = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffenseNameActionWI", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempIncidentOffenseNameActionWI
	#'
	#' This function returns a dataframe or json object of a TempIncidentOffenseNameActionWI
	#' @param TempIncidentOffenseNameActionWIID The ID of the TempIncidentOffenseNameActionWI to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentOffenseNameActionWI. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentOffenseNameActionWI.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentOffenseNameActionWI') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of TempIncidentOffenseNameActionWI
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempIncidentOffenseNameActionWI <- function(TempIncidentOffenseNameActionWIID, StateModifiedTermWIID = F, HasEarlyReinstatementCondition = F, BehaviorDetailedDescription = F, IAESRemovalType = F, CausedSeriousBodilyInjury = F, TempIncidentOffenseNameActionID = F, EntityID = F, FullName = F, StudentNumber = F, InvolvementType = F, OffenseCodeDescription = F, IsPrimaryOffense = F, ActionTypeID = F, ActionTypeCode = F, ActionID = F, ActionCodeDescription = F, Status = F, OrderedDate = F, DurationType = F, DurationToServe = F, StaffIDAuthorizedBy = F, StaffIDAuthorizedByName = F, StaffIDFollowUpOfficer = F, LocationID = F, BuildingID = F, RoomID = F, PerceivedMotivationCodeDescription = F, Comment = F, TempIncidentOffenseNameID = F, IsGuardianNotified = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartTime = F, InternalComment = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempIncidentOffenseNameActionWIID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameActionWI", objectId = TempIncidentOffenseNameActionWIID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempIncidentOffenseNameActionWI
	#'
	#' This function deletes a TempIncidentOffenseNameActionWI
	#' @param TempIncidentOffenseNameActionWIID The ID of the TempIncidentOffenseNameActionWI to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The TempIncidentOffenseNameActionWIID of the deleted TempIncidentOffenseNameActionWI.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempIncidentOffenseNameActionWI <- function(TempIncidentOffenseNameActionWIID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameActionWI", objectId = TempIncidentOffenseNameActionWIID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempIncidentOffenseNameActionWI
	#'
	#' This function creates a TempIncidentOffenseNameActionWI
	#' @param fieldNames The field values to give the created TempIncidentOffenseNameActionWI. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created TempIncidentOffenseNameActionWI
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempIncidentOffenseNameActionWI <- function(StateModifiedTermWIID = NULL, HasEarlyReinstatementCondition = NULL, BehaviorDetailedDescription = NULL, IAESRemovalType = NULL, CausedSeriousBodilyInjury = NULL, EntityID = NULL, FullName = NULL, StudentNumber = NULL, InvolvementType = NULL, OffenseCodeDescription = NULL, IsPrimaryOffense = NULL, ActionTypeID = NULL, ActionTypeCode = NULL, ActionID = NULL, ActionCodeDescription = NULL, Status = NULL, OrderedDate = NULL, DurationType = NULL, DurationToServe = NULL, StaffIDAuthorizedBy = NULL, StaffIDAuthorizedByName = NULL, StaffIDFollowUpOfficer = NULL, LocationID = NULL, BuildingID = NULL, RoomID = NULL, PerceivedMotivationCodeDescription = NULL, Comment = NULL, TempIncidentOffenseNameID = NULL, IsGuardianNotified = NULL, StartTime = NULL, InternalComment = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameActionWI", body = list(DataObject = body), searchFields = append("TempIncidentOffenseNameActionWIID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempIncidentOffenseNameActionWI
	#'
	#' This function modifies a TempIncidentOffenseNameActionWI
	#' @param fieldNames The field values to give the modified TempIncidentOffenseNameActionWI. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified TempIncidentOffenseNameActionWI
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempIncidentOffenseNameActionWI <- function(TempIncidentOffenseNameActionWIID, StateModifiedTermWIID = NULL, HasEarlyReinstatementCondition = NULL, BehaviorDetailedDescription = NULL, IAESRemovalType = NULL, CausedSeriousBodilyInjury = NULL, EntityID = NULL, FullName = NULL, StudentNumber = NULL, InvolvementType = NULL, OffenseCodeDescription = NULL, IsPrimaryOffense = NULL, ActionTypeID = NULL, ActionTypeCode = NULL, ActionID = NULL, ActionCodeDescription = NULL, Status = NULL, OrderedDate = NULL, DurationType = NULL, DurationToServe = NULL, StaffIDAuthorizedBy = NULL, StaffIDAuthorizedByName = NULL, StaffIDFollowUpOfficer = NULL, LocationID = NULL, BuildingID = NULL, RoomID = NULL, PerceivedMotivationCodeDescription = NULL, Comment = NULL, TempIncidentOffenseNameID = NULL, IsGuardianNotified = NULL, StartTime = NULL, InternalComment = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameActionWI", objectId = TempIncidentOffenseNameActionWIID, body = list(DataObject = body), searchFields = append("TempIncidentOffenseNameActionWIID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempIncidentOffenseNameActionWAS
	#'
	#' This function returns a dataframe or json object of TempIncidentOffenseNameActionWAS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentOffenseNameActionWAS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentOffenseNameActionWAS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentOffenseNameActionWA') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of TempIncidentOffenseNameActionWAS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempIncidentOffenseNameActionWAS <- function(searchConditionsList = NULL, PlacedInInterimAlternativeEducationalSetting = F, DateReadmissionPetitionSubmitted = F, DateReadmissionPetitionGranted = F, DateReengagementMeetingHeld = F, StateAcademicServiceWAID = F, StateBehaviorServiceWAID = F, StatePetitionExtensionExpulsionWAID = F, StateReengagementPlanWAID = F, StateAppealCodeWAID = F, TotalAmountOfExclusionaryTime = F, DurationOfExclusionaryActionDays = F, TempIncidentOffenseNameActionID = F, EntityID = F, FullName = F, StudentNumber = F, InvolvementType = F, OffenseCodeDescription = F, IsPrimaryOffense = F, ActionTypeID = F, ActionTypeCode = F, ActionID = F, ActionCodeDescription = F, Status = F, OrderedDate = F, DurationType = F, DurationToServe = F, StaffIDAuthorizedBy = F, StaffIDAuthorizedByName = F, StaffIDFollowUpOfficer = F, LocationID = F, BuildingID = F, RoomID = F, PerceivedMotivationCodeDescription = F, Comment = F, TempIncidentOffenseNameID = F, IsGuardianNotified = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartTime = F, InternalComment = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffenseNameActionWA", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempIncidentOffenseNameActionWA
	#'
	#' This function returns a dataframe or json object of a TempIncidentOffenseNameActionWA
	#' @param TempIncidentOffenseNameActionWAID The ID of the TempIncidentOffenseNameActionWA to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentOffenseNameActionWA. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentOffenseNameActionWA.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentOffenseNameActionWA') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of TempIncidentOffenseNameActionWA
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempIncidentOffenseNameActionWA <- function(TempIncidentOffenseNameActionWAID, PlacedInInterimAlternativeEducationalSetting = F, DateReadmissionPetitionSubmitted = F, DateReadmissionPetitionGranted = F, DateReengagementMeetingHeld = F, StateAcademicServiceWAID = F, StateBehaviorServiceWAID = F, StatePetitionExtensionExpulsionWAID = F, StateReengagementPlanWAID = F, StateAppealCodeWAID = F, TotalAmountOfExclusionaryTime = F, DurationOfExclusionaryActionDays = F, TempIncidentOffenseNameActionID = F, EntityID = F, FullName = F, StudentNumber = F, InvolvementType = F, OffenseCodeDescription = F, IsPrimaryOffense = F, ActionTypeID = F, ActionTypeCode = F, ActionID = F, ActionCodeDescription = F, Status = F, OrderedDate = F, DurationType = F, DurationToServe = F, StaffIDAuthorizedBy = F, StaffIDAuthorizedByName = F, StaffIDFollowUpOfficer = F, LocationID = F, BuildingID = F, RoomID = F, PerceivedMotivationCodeDescription = F, Comment = F, TempIncidentOffenseNameID = F, IsGuardianNotified = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartTime = F, InternalComment = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempIncidentOffenseNameActionWAID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameActionWA", objectId = TempIncidentOffenseNameActionWAID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempIncidentOffenseNameActionWA
	#'
	#' This function deletes a TempIncidentOffenseNameActionWA
	#' @param TempIncidentOffenseNameActionWAID The ID of the TempIncidentOffenseNameActionWA to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The TempIncidentOffenseNameActionWAID of the deleted TempIncidentOffenseNameActionWA.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempIncidentOffenseNameActionWA <- function(TempIncidentOffenseNameActionWAID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameActionWA", objectId = TempIncidentOffenseNameActionWAID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempIncidentOffenseNameActionWA
	#'
	#' This function creates a TempIncidentOffenseNameActionWA
	#' @param fieldNames The field values to give the created TempIncidentOffenseNameActionWA. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created TempIncidentOffenseNameActionWA
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempIncidentOffenseNameActionWA <- function(PlacedInInterimAlternativeEducationalSetting = NULL, DateReadmissionPetitionSubmitted = NULL, DateReadmissionPetitionGranted = NULL, DateReengagementMeetingHeld = NULL, StateAcademicServiceWAID = NULL, StateBehaviorServiceWAID = NULL, StatePetitionExtensionExpulsionWAID = NULL, StateReengagementPlanWAID = NULL, StateAppealCodeWAID = NULL, TotalAmountOfExclusionaryTime = NULL, DurationOfExclusionaryActionDays = NULL, EntityID = NULL, FullName = NULL, StudentNumber = NULL, InvolvementType = NULL, OffenseCodeDescription = NULL, IsPrimaryOffense = NULL, ActionTypeID = NULL, ActionTypeCode = NULL, ActionID = NULL, ActionCodeDescription = NULL, Status = NULL, OrderedDate = NULL, DurationType = NULL, DurationToServe = NULL, StaffIDAuthorizedBy = NULL, StaffIDAuthorizedByName = NULL, StaffIDFollowUpOfficer = NULL, LocationID = NULL, BuildingID = NULL, RoomID = NULL, PerceivedMotivationCodeDescription = NULL, Comment = NULL, TempIncidentOffenseNameID = NULL, IsGuardianNotified = NULL, StartTime = NULL, InternalComment = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameActionWA", body = list(DataObject = body), searchFields = append("TempIncidentOffenseNameActionWAID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempIncidentOffenseNameActionWA
	#'
	#' This function modifies a TempIncidentOffenseNameActionWA
	#' @param fieldNames The field values to give the modified TempIncidentOffenseNameActionWA. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified TempIncidentOffenseNameActionWA
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempIncidentOffenseNameActionWA <- function(TempIncidentOffenseNameActionWAID, PlacedInInterimAlternativeEducationalSetting = NULL, DateReadmissionPetitionSubmitted = NULL, DateReadmissionPetitionGranted = NULL, DateReengagementMeetingHeld = NULL, StateAcademicServiceWAID = NULL, StateBehaviorServiceWAID = NULL, StatePetitionExtensionExpulsionWAID = NULL, StateReengagementPlanWAID = NULL, StateAppealCodeWAID = NULL, TotalAmountOfExclusionaryTime = NULL, DurationOfExclusionaryActionDays = NULL, EntityID = NULL, FullName = NULL, StudentNumber = NULL, InvolvementType = NULL, OffenseCodeDescription = NULL, IsPrimaryOffense = NULL, ActionTypeID = NULL, ActionTypeCode = NULL, ActionID = NULL, ActionCodeDescription = NULL, Status = NULL, OrderedDate = NULL, DurationType = NULL, DurationToServe = NULL, StaffIDAuthorizedBy = NULL, StaffIDAuthorizedByName = NULL, StaffIDFollowUpOfficer = NULL, LocationID = NULL, BuildingID = NULL, RoomID = NULL, PerceivedMotivationCodeDescription = NULL, Comment = NULL, TempIncidentOffenseNameID = NULL, IsGuardianNotified = NULL, StartTime = NULL, InternalComment = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameActionWA", objectId = TempIncidentOffenseNameActionWAID, body = list(DataObject = body), searchFields = append("TempIncidentOffenseNameActionWAID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempIncidentOffenseNameActionPAS
	#'
	#' This function returns a dataframe or json object of TempIncidentOffenseNameActionPAS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentOffenseNameActionPAS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentOffenseNameActionPAS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentOffenseNameActionPA') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of TempIncidentOffenseNameActionPAS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempIncidentOffenseNameActionPAS <- function(searchConditionsList = NULL, ReceivedEducationalServices = F, AssignedAlternativeEducation = F, TempIncidentOffenseNameActionID = F, EntityID = F, FullName = F, StudentNumber = F, InvolvementType = F, OffenseCodeDescription = F, IsPrimaryOffense = F, ActionTypeID = F, ActionTypeCode = F, ActionID = F, ActionCodeDescription = F, Status = F, OrderedDate = F, DurationType = F, DurationToServe = F, StaffIDAuthorizedBy = F, StaffIDAuthorizedByName = F, StaffIDFollowUpOfficer = F, LocationID = F, BuildingID = F, RoomID = F, PerceivedMotivationCodeDescription = F, Comment = F, TempIncidentOffenseNameID = F, IsGuardianNotified = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartTime = F, InternalComment = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffenseNameActionPA", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempIncidentOffenseNameActionPA
	#'
	#' This function returns a dataframe or json object of a TempIncidentOffenseNameActionPA
	#' @param TempIncidentOffenseNameActionPAID The ID of the TempIncidentOffenseNameActionPA to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentOffenseNameActionPA. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentOffenseNameActionPA.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentOffenseNameActionPA') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of TempIncidentOffenseNameActionPA
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempIncidentOffenseNameActionPA <- function(TempIncidentOffenseNameActionPAID, ReceivedEducationalServices = F, AssignedAlternativeEducation = F, TempIncidentOffenseNameActionID = F, EntityID = F, FullName = F, StudentNumber = F, InvolvementType = F, OffenseCodeDescription = F, IsPrimaryOffense = F, ActionTypeID = F, ActionTypeCode = F, ActionID = F, ActionCodeDescription = F, Status = F, OrderedDate = F, DurationType = F, DurationToServe = F, StaffIDAuthorizedBy = F, StaffIDAuthorizedByName = F, StaffIDFollowUpOfficer = F, LocationID = F, BuildingID = F, RoomID = F, PerceivedMotivationCodeDescription = F, Comment = F, TempIncidentOffenseNameID = F, IsGuardianNotified = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartTime = F, InternalComment = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempIncidentOffenseNameActionPAID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameActionPA", objectId = TempIncidentOffenseNameActionPAID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempIncidentOffenseNameActionPA
	#'
	#' This function deletes a TempIncidentOffenseNameActionPA
	#' @param TempIncidentOffenseNameActionPAID The ID of the TempIncidentOffenseNameActionPA to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The TempIncidentOffenseNameActionPAID of the deleted TempIncidentOffenseNameActionPA.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempIncidentOffenseNameActionPA <- function(TempIncidentOffenseNameActionPAID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameActionPA", objectId = TempIncidentOffenseNameActionPAID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempIncidentOffenseNameActionPA
	#'
	#' This function creates a TempIncidentOffenseNameActionPA
	#' @param fieldNames The field values to give the created TempIncidentOffenseNameActionPA. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created TempIncidentOffenseNameActionPA
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempIncidentOffenseNameActionPA <- function(ReceivedEducationalServices = NULL, AssignedAlternativeEducation = NULL, EntityID = NULL, FullName = NULL, StudentNumber = NULL, InvolvementType = NULL, OffenseCodeDescription = NULL, IsPrimaryOffense = NULL, ActionTypeID = NULL, ActionTypeCode = NULL, ActionID = NULL, ActionCodeDescription = NULL, Status = NULL, OrderedDate = NULL, DurationType = NULL, DurationToServe = NULL, StaffIDAuthorizedBy = NULL, StaffIDAuthorizedByName = NULL, StaffIDFollowUpOfficer = NULL, LocationID = NULL, BuildingID = NULL, RoomID = NULL, PerceivedMotivationCodeDescription = NULL, Comment = NULL, TempIncidentOffenseNameID = NULL, IsGuardianNotified = NULL, StartTime = NULL, InternalComment = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameActionPA", body = list(DataObject = body), searchFields = append("TempIncidentOffenseNameActionPAID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempIncidentOffenseNameActionPA
	#'
	#' This function modifies a TempIncidentOffenseNameActionPA
	#' @param fieldNames The field values to give the modified TempIncidentOffenseNameActionPA. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified TempIncidentOffenseNameActionPA
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempIncidentOffenseNameActionPA <- function(TempIncidentOffenseNameActionPAID, ReceivedEducationalServices = NULL, AssignedAlternativeEducation = NULL, EntityID = NULL, FullName = NULL, StudentNumber = NULL, InvolvementType = NULL, OffenseCodeDescription = NULL, IsPrimaryOffense = NULL, ActionTypeID = NULL, ActionTypeCode = NULL, ActionID = NULL, ActionCodeDescription = NULL, Status = NULL, OrderedDate = NULL, DurationType = NULL, DurationToServe = NULL, StaffIDAuthorizedBy = NULL, StaffIDAuthorizedByName = NULL, StaffIDFollowUpOfficer = NULL, LocationID = NULL, BuildingID = NULL, RoomID = NULL, PerceivedMotivationCodeDescription = NULL, Comment = NULL, TempIncidentOffenseNameID = NULL, IsGuardianNotified = NULL, StartTime = NULL, InternalComment = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameActionPA", objectId = TempIncidentOffenseNameActionPAID, body = list(DataObject = body), searchFields = append("TempIncidentOffenseNameActionPAID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempIncidentOffenseNameActionMNS
	#'
	#' This function returns a dataframe or json object of TempIncidentOffenseNameActionMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentOffenseNameActionMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentOffenseNameActionMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentOffenseNameActionMN') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of TempIncidentOffenseNameActionMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempIncidentOffenseNameActionMNS <- function(searchConditionsList = NULL, StateDIRSAESTypeMNID = F, DateExpulsionExclusionEnds = F, ReturnBeforeYearEnd = F, ExclusionThroughYearEnd = F, ExpulsionModified = F, ExpulsionThroughYearEnd = F, DIRSActionExplanation = F, TempIncidentOffenseNameActionID = F, EntityID = F, FullName = F, StudentNumber = F, InvolvementType = F, OffenseCodeDescription = F, IsPrimaryOffense = F, ActionTypeID = F, ActionTypeCode = F, ActionID = F, ActionCodeDescription = F, Status = F, OrderedDate = F, DurationType = F, DurationToServe = F, StaffIDAuthorizedBy = F, StaffIDAuthorizedByName = F, StaffIDFollowUpOfficer = F, LocationID = F, BuildingID = F, RoomID = F, PerceivedMotivationCodeDescription = F, Comment = F, TempIncidentOffenseNameID = F, IsGuardianNotified = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartTime = F, InternalComment = F, NoServiceProvidedExplanation = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffenseNameActionMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempIncidentOffenseNameActionMN
	#'
	#' This function returns a dataframe or json object of a TempIncidentOffenseNameActionMN
	#' @param TempIncidentOffenseNameActionMNID The ID of the TempIncidentOffenseNameActionMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentOffenseNameActionMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentOffenseNameActionMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentOffenseNameActionMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of TempIncidentOffenseNameActionMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempIncidentOffenseNameActionMN <- function(TempIncidentOffenseNameActionMNID, StateDIRSAESTypeMNID = F, DateExpulsionExclusionEnds = F, ReturnBeforeYearEnd = F, ExclusionThroughYearEnd = F, ExpulsionModified = F, ExpulsionThroughYearEnd = F, DIRSActionExplanation = F, TempIncidentOffenseNameActionID = F, EntityID = F, FullName = F, StudentNumber = F, InvolvementType = F, OffenseCodeDescription = F, IsPrimaryOffense = F, ActionTypeID = F, ActionTypeCode = F, ActionID = F, ActionCodeDescription = F, Status = F, OrderedDate = F, DurationType = F, DurationToServe = F, StaffIDAuthorizedBy = F, StaffIDAuthorizedByName = F, StaffIDFollowUpOfficer = F, LocationID = F, BuildingID = F, RoomID = F, PerceivedMotivationCodeDescription = F, Comment = F, TempIncidentOffenseNameID = F, IsGuardianNotified = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartTime = F, InternalComment = F, NoServiceProvidedExplanation = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempIncidentOffenseNameActionMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameActionMN", objectId = TempIncidentOffenseNameActionMNID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempIncidentOffenseNameActionMN
	#'
	#' This function deletes a TempIncidentOffenseNameActionMN
	#' @param TempIncidentOffenseNameActionMNID The ID of the TempIncidentOffenseNameActionMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The TempIncidentOffenseNameActionMNID of the deleted TempIncidentOffenseNameActionMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempIncidentOffenseNameActionMN <- function(TempIncidentOffenseNameActionMNID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameActionMN", objectId = TempIncidentOffenseNameActionMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempIncidentOffenseNameActionMN
	#'
	#' This function creates a TempIncidentOffenseNameActionMN
	#' @param fieldNames The field values to give the created TempIncidentOffenseNameActionMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created TempIncidentOffenseNameActionMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempIncidentOffenseNameActionMN <- function(StateDIRSAESTypeMNID = NULL, DateExpulsionExclusionEnds = NULL, ReturnBeforeYearEnd = NULL, ExclusionThroughYearEnd = NULL, ExpulsionModified = NULL, ExpulsionThroughYearEnd = NULL, DIRSActionExplanation = NULL, EntityID = NULL, FullName = NULL, StudentNumber = NULL, InvolvementType = NULL, OffenseCodeDescription = NULL, IsPrimaryOffense = NULL, ActionTypeID = NULL, ActionTypeCode = NULL, ActionID = NULL, ActionCodeDescription = NULL, Status = NULL, OrderedDate = NULL, DurationType = NULL, DurationToServe = NULL, StaffIDAuthorizedBy = NULL, StaffIDAuthorizedByName = NULL, StaffIDFollowUpOfficer = NULL, LocationID = NULL, BuildingID = NULL, RoomID = NULL, PerceivedMotivationCodeDescription = NULL, Comment = NULL, TempIncidentOffenseNameID = NULL, IsGuardianNotified = NULL, StartTime = NULL, InternalComment = NULL, NoServiceProvidedExplanation = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameActionMN", body = list(DataObject = body), searchFields = append("TempIncidentOffenseNameActionMNID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempIncidentOffenseNameActionMN
	#'
	#' This function modifies a TempIncidentOffenseNameActionMN
	#' @param fieldNames The field values to give the modified TempIncidentOffenseNameActionMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified TempIncidentOffenseNameActionMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempIncidentOffenseNameActionMN <- function(TempIncidentOffenseNameActionMNID, StateDIRSAESTypeMNID = NULL, DateExpulsionExclusionEnds = NULL, ReturnBeforeYearEnd = NULL, ExclusionThroughYearEnd = NULL, ExpulsionModified = NULL, ExpulsionThroughYearEnd = NULL, DIRSActionExplanation = NULL, EntityID = NULL, FullName = NULL, StudentNumber = NULL, InvolvementType = NULL, OffenseCodeDescription = NULL, IsPrimaryOffense = NULL, ActionTypeID = NULL, ActionTypeCode = NULL, ActionID = NULL, ActionCodeDescription = NULL, Status = NULL, OrderedDate = NULL, DurationType = NULL, DurationToServe = NULL, StaffIDAuthorizedBy = NULL, StaffIDAuthorizedByName = NULL, StaffIDFollowUpOfficer = NULL, LocationID = NULL, BuildingID = NULL, RoomID = NULL, PerceivedMotivationCodeDescription = NULL, Comment = NULL, TempIncidentOffenseNameID = NULL, IsGuardianNotified = NULL, StartTime = NULL, InternalComment = NULL, NoServiceProvidedExplanation = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameActionMN", objectId = TempIncidentOffenseNameActionMNID, body = list(DataObject = body), searchFields = append("TempIncidentOffenseNameActionMNID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempIncidentOffenseNameActionINS
	#'
	#' This function returns a dataframe or json object of TempIncidentOffenseNameActionINS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentOffenseNameActionINS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentOffenseNameActionINS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentOffenseNameActionIN') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of TempIncidentOffenseNameActionINS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempIncidentOffenseNameActionINS <- function(searchConditionsList = NULL, StateSuspensionReasonINID = F, StateEducationalServiceProvidedINID = F, TempIncidentOffenseNameActionID = F, EntityID = F, FullName = F, StudentNumber = F, InvolvementType = F, OffenseCodeDescription = F, IsPrimaryOffense = F, ActionTypeID = F, ActionTypeCode = F, ActionID = F, ActionCodeDescription = F, Status = F, OrderedDate = F, DurationType = F, DurationToServe = F, StaffIDAuthorizedBy = F, StaffIDAuthorizedByName = F, StaffIDFollowUpOfficer = F, LocationID = F, BuildingID = F, RoomID = F, PerceivedMotivationCodeDescription = F, Comment = F, TempIncidentOffenseNameID = F, IsGuardianNotified = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartTime = F, IsPrimaryAction = F, InternalComment = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffenseNameActionIN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempIncidentOffenseNameActionIN
	#'
	#' This function returns a dataframe or json object of a TempIncidentOffenseNameActionIN
	#' @param TempIncidentOffenseNameActionINID The ID of the TempIncidentOffenseNameActionIN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentOffenseNameActionIN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentOffenseNameActionIN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentOffenseNameActionIN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of TempIncidentOffenseNameActionIN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempIncidentOffenseNameActionIN <- function(TempIncidentOffenseNameActionINID, StateSuspensionReasonINID = F, StateEducationalServiceProvidedINID = F, TempIncidentOffenseNameActionID = F, EntityID = F, FullName = F, StudentNumber = F, InvolvementType = F, OffenseCodeDescription = F, IsPrimaryOffense = F, ActionTypeID = F, ActionTypeCode = F, ActionID = F, ActionCodeDescription = F, Status = F, OrderedDate = F, DurationType = F, DurationToServe = F, StaffIDAuthorizedBy = F, StaffIDAuthorizedByName = F, StaffIDFollowUpOfficer = F, LocationID = F, BuildingID = F, RoomID = F, PerceivedMotivationCodeDescription = F, Comment = F, TempIncidentOffenseNameID = F, IsGuardianNotified = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartTime = F, IsPrimaryAction = F, InternalComment = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempIncidentOffenseNameActionINID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameActionIN", objectId = TempIncidentOffenseNameActionINID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempIncidentOffenseNameActionIN
	#'
	#' This function deletes a TempIncidentOffenseNameActionIN
	#' @param TempIncidentOffenseNameActionINID The ID of the TempIncidentOffenseNameActionIN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The TempIncidentOffenseNameActionINID of the deleted TempIncidentOffenseNameActionIN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempIncidentOffenseNameActionIN <- function(TempIncidentOffenseNameActionINID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameActionIN", objectId = TempIncidentOffenseNameActionINID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempIncidentOffenseNameActionIN
	#'
	#' This function creates a TempIncidentOffenseNameActionIN
	#' @param fieldNames The field values to give the created TempIncidentOffenseNameActionIN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created TempIncidentOffenseNameActionIN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempIncidentOffenseNameActionIN <- function(StateSuspensionReasonINID = NULL, StateEducationalServiceProvidedINID = NULL, EntityID = NULL, FullName = NULL, StudentNumber = NULL, InvolvementType = NULL, OffenseCodeDescription = NULL, IsPrimaryOffense = NULL, ActionTypeID = NULL, ActionTypeCode = NULL, ActionID = NULL, ActionCodeDescription = NULL, Status = NULL, OrderedDate = NULL, DurationType = NULL, DurationToServe = NULL, StaffIDAuthorizedBy = NULL, StaffIDAuthorizedByName = NULL, StaffIDFollowUpOfficer = NULL, LocationID = NULL, BuildingID = NULL, RoomID = NULL, PerceivedMotivationCodeDescription = NULL, Comment = NULL, TempIncidentOffenseNameID = NULL, IsGuardianNotified = NULL, StartTime = NULL, IsPrimaryAction = NULL, InternalComment = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameActionIN", body = list(DataObject = body), searchFields = append("TempIncidentOffenseNameActionINID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempIncidentOffenseNameActionIN
	#'
	#' This function modifies a TempIncidentOffenseNameActionIN
	#' @param fieldNames The field values to give the modified TempIncidentOffenseNameActionIN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified TempIncidentOffenseNameActionIN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempIncidentOffenseNameActionIN <- function(TempIncidentOffenseNameActionINID, StateSuspensionReasonINID = NULL, StateEducationalServiceProvidedINID = NULL, EntityID = NULL, FullName = NULL, StudentNumber = NULL, InvolvementType = NULL, OffenseCodeDescription = NULL, IsPrimaryOffense = NULL, ActionTypeID = NULL, ActionTypeCode = NULL, ActionID = NULL, ActionCodeDescription = NULL, Status = NULL, OrderedDate = NULL, DurationType = NULL, DurationToServe = NULL, StaffIDAuthorizedBy = NULL, StaffIDAuthorizedByName = NULL, StaffIDFollowUpOfficer = NULL, LocationID = NULL, BuildingID = NULL, RoomID = NULL, PerceivedMotivationCodeDescription = NULL, Comment = NULL, TempIncidentOffenseNameID = NULL, IsGuardianNotified = NULL, StartTime = NULL, IsPrimaryAction = NULL, InternalComment = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameActionIN", objectId = TempIncidentOffenseNameActionINID, body = list(DataObject = body), searchFields = append("TempIncidentOffenseNameActionINID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempIncidentInvolvedPersonWAS
	#'
	#' This function returns a dataframe or json object of TempIncidentInvolvedPersonWAS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentInvolvedPersonWAS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentInvolvedPersonWAS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentInvolvedPersonWA') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of TempIncidentInvolvedPersonWAS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempIncidentInvolvedPersonWAS <- function(searchConditionsList = NULL, StateReportedOffense = F, TempIncidentInvolvedPersonID = F, IncidentOffenseNameKey = F, PrimaryOffenseID = F, SecondaryOffensesBackingData = F, ExcludePrimaryOffense = F, ExistingIncidentOffenseNamesToDelete = F, InvolvementType = F, IncidentOffenseNameType = F, FreeformName = F, StudentID = F, StaffID = F, NameID = F, StaffIDDisciplineOfficer = F, PerceivedMotivationID = F, FullName = F, StudentNumber = F, PerceivedMotivationCodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OSSCount = F, OSSPartialCount = F, ISSCount = F, ISSPartialCount = F, InternalComment = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentInvolvedPersonWA", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempIncidentInvolvedPersonWA
	#'
	#' This function returns a dataframe or json object of a TempIncidentInvolvedPersonWA
	#' @param TempIncidentInvolvedPersonWAID The ID of the TempIncidentInvolvedPersonWA to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentInvolvedPersonWA. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentInvolvedPersonWA.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentInvolvedPersonWA') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of TempIncidentInvolvedPersonWA
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempIncidentInvolvedPersonWA <- function(TempIncidentInvolvedPersonWAID, StateReportedOffense = F, TempIncidentInvolvedPersonID = F, IncidentOffenseNameKey = F, PrimaryOffenseID = F, SecondaryOffensesBackingData = F, ExcludePrimaryOffense = F, ExistingIncidentOffenseNamesToDelete = F, InvolvementType = F, IncidentOffenseNameType = F, FreeformName = F, StudentID = F, StaffID = F, NameID = F, StaffIDDisciplineOfficer = F, PerceivedMotivationID = F, FullName = F, StudentNumber = F, PerceivedMotivationCodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OSSCount = F, OSSPartialCount = F, ISSCount = F, ISSPartialCount = F, InternalComment = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempIncidentInvolvedPersonWAID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "TempIncidentInvolvedPersonWA", objectId = TempIncidentInvolvedPersonWAID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempIncidentInvolvedPersonWA
	#'
	#' This function deletes a TempIncidentInvolvedPersonWA
	#' @param TempIncidentInvolvedPersonWAID The ID of the TempIncidentInvolvedPersonWA to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The TempIncidentInvolvedPersonWAID of the deleted TempIncidentInvolvedPersonWA.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempIncidentInvolvedPersonWA <- function(TempIncidentInvolvedPersonWAID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "TempIncidentInvolvedPersonWA", objectId = TempIncidentInvolvedPersonWAID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempIncidentInvolvedPersonWA
	#'
	#' This function creates a TempIncidentInvolvedPersonWA
	#' @param fieldNames The field values to give the created TempIncidentInvolvedPersonWA. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created TempIncidentInvolvedPersonWA
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempIncidentInvolvedPersonWA <- function(StateReportedOffense = NULL, IncidentOffenseNameKey = NULL, PrimaryOffenseID = NULL, ExcludePrimaryOffense = NULL, InvolvementType = NULL, IncidentOffenseNameType = NULL, FreeformName = NULL, StudentID = NULL, StaffID = NULL, NameID = NULL, StaffIDDisciplineOfficer = NULL, PerceivedMotivationID = NULL, FullName = NULL, StudentNumber = NULL, PerceivedMotivationCodeDescription = NULL, OSSCount = NULL, OSSPartialCount = NULL, ISSCount = NULL, ISSPartialCount = NULL, InternalComment = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "TempIncidentInvolvedPersonWA", body = list(DataObject = body), searchFields = append("TempIncidentInvolvedPersonWAID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempIncidentInvolvedPersonWA
	#'
	#' This function modifies a TempIncidentInvolvedPersonWA
	#' @param fieldNames The field values to give the modified TempIncidentInvolvedPersonWA. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified TempIncidentInvolvedPersonWA
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempIncidentInvolvedPersonWA <- function(TempIncidentInvolvedPersonWAID, StateReportedOffense = NULL, IncidentOffenseNameKey = NULL, PrimaryOffenseID = NULL, ExcludePrimaryOffense = NULL, InvolvementType = NULL, IncidentOffenseNameType = NULL, FreeformName = NULL, StudentID = NULL, StaffID = NULL, NameID = NULL, StaffIDDisciplineOfficer = NULL, PerceivedMotivationID = NULL, FullName = NULL, StudentNumber = NULL, PerceivedMotivationCodeDescription = NULL, OSSCount = NULL, OSSPartialCount = NULL, ISSCount = NULL, ISSPartialCount = NULL, InternalComment = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "TempIncidentInvolvedPersonWA", objectId = TempIncidentInvolvedPersonWAID, body = list(DataObject = body), searchFields = append("TempIncidentInvolvedPersonWAID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempIncidentInvolvedPersonTXES
	#'
	#' This function returns a dataframe or json object of TempIncidentInvolvedPersonTXES
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentInvolvedPersonTXES. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentInvolvedPersonTXES.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentInvolvedPersonTX') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of TempIncidentInvolvedPersonTXES
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempIncidentInvolvedPersonTXES <- function(searchConditionsList = NULL, CampusIDOfDisciplinaryResponsibility = F, TempIncidentInvolvedPersonID = F, IncidentOffenseNameKey = F, PrimaryOffenseID = F, SecondaryOffensesBackingData = F, ExcludePrimaryOffense = F, ExistingIncidentOffenseNamesToDelete = F, InvolvementType = F, IncidentOffenseNameType = F, FreeformName = F, StudentID = F, StaffID = F, NameID = F, StaffIDDisciplineOfficer = F, PerceivedMotivationID = F, FullName = F, StudentNumber = F, PerceivedMotivationCodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OSSCount = F, OSSPartialCount = F, ISSCount = F, ISSPartialCount = F, InternalComment = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentInvolvedPersonTX", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempIncidentInvolvedPersonTX
	#'
	#' This function returns a dataframe or json object of a TempIncidentInvolvedPersonTX
	#' @param TempIncidentInvolvedPersonTXID The ID of the TempIncidentInvolvedPersonTX to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentInvolvedPersonTX. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentInvolvedPersonTX.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentInvolvedPersonTX') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of TempIncidentInvolvedPersonTX
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempIncidentInvolvedPersonTX <- function(TempIncidentInvolvedPersonTXID, CampusIDOfDisciplinaryResponsibility = F, TempIncidentInvolvedPersonID = F, IncidentOffenseNameKey = F, PrimaryOffenseID = F, SecondaryOffensesBackingData = F, ExcludePrimaryOffense = F, ExistingIncidentOffenseNamesToDelete = F, InvolvementType = F, IncidentOffenseNameType = F, FreeformName = F, StudentID = F, StaffID = F, NameID = F, StaffIDDisciplineOfficer = F, PerceivedMotivationID = F, FullName = F, StudentNumber = F, PerceivedMotivationCodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OSSCount = F, OSSPartialCount = F, ISSCount = F, ISSPartialCount = F, InternalComment = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempIncidentInvolvedPersonTXID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "TempIncidentInvolvedPersonTX", objectId = TempIncidentInvolvedPersonTXID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempIncidentInvolvedPersonTX
	#'
	#' This function deletes a TempIncidentInvolvedPersonTX
	#' @param TempIncidentInvolvedPersonTXID The ID of the TempIncidentInvolvedPersonTX to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The TempIncidentInvolvedPersonTXID of the deleted TempIncidentInvolvedPersonTX.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempIncidentInvolvedPersonTX <- function(TempIncidentInvolvedPersonTXID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "TempIncidentInvolvedPersonTX", objectId = TempIncidentInvolvedPersonTXID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempIncidentInvolvedPersonTX
	#'
	#' This function creates a TempIncidentInvolvedPersonTX
	#' @param fieldNames The field values to give the created TempIncidentInvolvedPersonTX. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created TempIncidentInvolvedPersonTX
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempIncidentInvolvedPersonTX <- function(CampusIDOfDisciplinaryResponsibility = NULL, IncidentOffenseNameKey = NULL, PrimaryOffenseID = NULL, ExcludePrimaryOffense = NULL, InvolvementType = NULL, IncidentOffenseNameType = NULL, FreeformName = NULL, StudentID = NULL, StaffID = NULL, NameID = NULL, StaffIDDisciplineOfficer = NULL, PerceivedMotivationID = NULL, FullName = NULL, StudentNumber = NULL, PerceivedMotivationCodeDescription = NULL, OSSCount = NULL, OSSPartialCount = NULL, ISSCount = NULL, ISSPartialCount = NULL, InternalComment = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "TempIncidentInvolvedPersonTX", body = list(DataObject = body), searchFields = append("TempIncidentInvolvedPersonTXID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempIncidentInvolvedPersonTX
	#'
	#' This function modifies a TempIncidentInvolvedPersonTX
	#' @param fieldNames The field values to give the modified TempIncidentInvolvedPersonTX. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified TempIncidentInvolvedPersonTX
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempIncidentInvolvedPersonTX <- function(TempIncidentInvolvedPersonTXID, CampusIDOfDisciplinaryResponsibility = NULL, IncidentOffenseNameKey = NULL, PrimaryOffenseID = NULL, ExcludePrimaryOffense = NULL, InvolvementType = NULL, IncidentOffenseNameType = NULL, FreeformName = NULL, StudentID = NULL, StaffID = NULL, NameID = NULL, StaffIDDisciplineOfficer = NULL, PerceivedMotivationID = NULL, FullName = NULL, StudentNumber = NULL, PerceivedMotivationCodeDescription = NULL, OSSCount = NULL, OSSPartialCount = NULL, ISSCount = NULL, ISSPartialCount = NULL, InternalComment = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "TempIncidentInvolvedPersonTX", objectId = TempIncidentInvolvedPersonTXID, body = list(DataObject = body), searchFields = append("TempIncidentInvolvedPersonTXID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempIncidentInvolvedPersonPAS
	#'
	#' This function returns a dataframe or json object of TempIncidentInvolvedPersonPAS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentInvolvedPersonPAS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentInvolvedPersonPAS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentInvolvedPersonPA') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of TempIncidentInvolvedPersonPAS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempIncidentInvolvedPersonPAS <- function(searchConditionsList = NULL, StateVictimTypePAID = F, StudentAssistanceProgramReferral = F, MedicalTreatmentRequired = F, IncidentVictimComment = F, StateInjurySeverityPAID = F, StateOffenderTypePAID = F, StateArrestedPAID = F, StateAdjudicationPAID = F, StateWeaponDetectedMethodPAID = F, WeaponDetectionComment = F, LocalLawEnforcementNotified = F, NameOfLocalLawEnforcementContacted = F, TempIncidentInvolvedPersonID = F, IncidentOffenseNameKey = F, PrimaryOffenseID = F, SecondaryOffensesBackingData = F, ExcludePrimaryOffense = F, ExistingIncidentOffenseNamesToDelete = F, InvolvementType = F, IncidentOffenseNameType = F, FreeformName = F, StudentID = F, StaffID = F, NameID = F, StaffIDDisciplineOfficer = F, PerceivedMotivationID = F, FullName = F, StudentNumber = F, PerceivedMotivationCodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsResidentialPlacementByNonEdAgency = F, LLEIncidentNumber = F, OSSCount = F, OSSPartialCount = F, ISSCount = F, ISSPartialCount = F, InternalComment = F, StateDistrictPAIDPerson = F, PASecureID = F, FirstName = F, LastName = F, RaceEthnicities = F, Genders = F, SendingOrCharterTypes = F, SendingDistrictOrCharterAUNID = F, AgeAtTimeOfIncident = F, StateGradeLevelPIMSPAID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentInvolvedPersonPA", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempIncidentInvolvedPersonPA
	#'
	#' This function returns a dataframe or json object of a TempIncidentInvolvedPersonPA
	#' @param TempIncidentInvolvedPersonPAID The ID of the TempIncidentInvolvedPersonPA to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentInvolvedPersonPA. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentInvolvedPersonPA.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentInvolvedPersonPA') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of TempIncidentInvolvedPersonPA
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempIncidentInvolvedPersonPA <- function(TempIncidentInvolvedPersonPAID, StateVictimTypePAID = F, StudentAssistanceProgramReferral = F, MedicalTreatmentRequired = F, IncidentVictimComment = F, StateInjurySeverityPAID = F, StateOffenderTypePAID = F, StateArrestedPAID = F, StateAdjudicationPAID = F, StateWeaponDetectedMethodPAID = F, WeaponDetectionComment = F, LocalLawEnforcementNotified = F, NameOfLocalLawEnforcementContacted = F, TempIncidentInvolvedPersonID = F, IncidentOffenseNameKey = F, PrimaryOffenseID = F, SecondaryOffensesBackingData = F, ExcludePrimaryOffense = F, ExistingIncidentOffenseNamesToDelete = F, InvolvementType = F, IncidentOffenseNameType = F, FreeformName = F, StudentID = F, StaffID = F, NameID = F, StaffIDDisciplineOfficer = F, PerceivedMotivationID = F, FullName = F, StudentNumber = F, PerceivedMotivationCodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsResidentialPlacementByNonEdAgency = F, LLEIncidentNumber = F, OSSCount = F, OSSPartialCount = F, ISSCount = F, ISSPartialCount = F, InternalComment = F, StateDistrictPAIDPerson = F, PASecureID = F, FirstName = F, LastName = F, RaceEthnicities = F, Genders = F, SendingOrCharterTypes = F, SendingDistrictOrCharterAUNID = F, AgeAtTimeOfIncident = F, StateGradeLevelPIMSPAID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempIncidentInvolvedPersonPAID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "TempIncidentInvolvedPersonPA", objectId = TempIncidentInvolvedPersonPAID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempIncidentInvolvedPersonPA
	#'
	#' This function deletes a TempIncidentInvolvedPersonPA
	#' @param TempIncidentInvolvedPersonPAID The ID of the TempIncidentInvolvedPersonPA to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The TempIncidentInvolvedPersonPAID of the deleted TempIncidentInvolvedPersonPA.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempIncidentInvolvedPersonPA <- function(TempIncidentInvolvedPersonPAID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "TempIncidentInvolvedPersonPA", objectId = TempIncidentInvolvedPersonPAID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempIncidentInvolvedPersonPA
	#'
	#' This function creates a TempIncidentInvolvedPersonPA
	#' @param fieldNames The field values to give the created TempIncidentInvolvedPersonPA. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created TempIncidentInvolvedPersonPA
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempIncidentInvolvedPersonPA <- function(StateVictimTypePAID = NULL, StudentAssistanceProgramReferral = NULL, MedicalTreatmentRequired = NULL, IncidentVictimComment = NULL, StateInjurySeverityPAID = NULL, StateOffenderTypePAID = NULL, StateArrestedPAID = NULL, StateAdjudicationPAID = NULL, StateWeaponDetectedMethodPAID = NULL, WeaponDetectionComment = NULL, LocalLawEnforcementNotified = NULL, NameOfLocalLawEnforcementContacted = NULL, IncidentOffenseNameKey = NULL, PrimaryOffenseID = NULL, ExcludePrimaryOffense = NULL, InvolvementType = NULL, IncidentOffenseNameType = NULL, FreeformName = NULL, StudentID = NULL, StaffID = NULL, NameID = NULL, StaffIDDisciplineOfficer = NULL, PerceivedMotivationID = NULL, FullName = NULL, StudentNumber = NULL, PerceivedMotivationCodeDescription = NULL, IsResidentialPlacementByNonEdAgency = NULL, LLEIncidentNumber = NULL, OSSCount = NULL, OSSPartialCount = NULL, ISSCount = NULL, ISSPartialCount = NULL, InternalComment = NULL, StateDistrictPAIDPerson = NULL, PASecureID = NULL, FirstName = NULL, LastName = NULL, RaceEthnicities = NULL, Genders = NULL, SendingOrCharterTypes = NULL, SendingDistrictOrCharterAUNID = NULL, AgeAtTimeOfIncident = NULL, StateGradeLevelPIMSPAID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "TempIncidentInvolvedPersonPA", body = list(DataObject = body), searchFields = append("TempIncidentInvolvedPersonPAID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempIncidentInvolvedPersonPA
	#'
	#' This function modifies a TempIncidentInvolvedPersonPA
	#' @param fieldNames The field values to give the modified TempIncidentInvolvedPersonPA. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified TempIncidentInvolvedPersonPA
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempIncidentInvolvedPersonPA <- function(TempIncidentInvolvedPersonPAID, StateVictimTypePAID = NULL, StudentAssistanceProgramReferral = NULL, MedicalTreatmentRequired = NULL, IncidentVictimComment = NULL, StateInjurySeverityPAID = NULL, StateOffenderTypePAID = NULL, StateArrestedPAID = NULL, StateAdjudicationPAID = NULL, StateWeaponDetectedMethodPAID = NULL, WeaponDetectionComment = NULL, LocalLawEnforcementNotified = NULL, NameOfLocalLawEnforcementContacted = NULL, IncidentOffenseNameKey = NULL, PrimaryOffenseID = NULL, ExcludePrimaryOffense = NULL, InvolvementType = NULL, IncidentOffenseNameType = NULL, FreeformName = NULL, StudentID = NULL, StaffID = NULL, NameID = NULL, StaffIDDisciplineOfficer = NULL, PerceivedMotivationID = NULL, FullName = NULL, StudentNumber = NULL, PerceivedMotivationCodeDescription = NULL, IsResidentialPlacementByNonEdAgency = NULL, LLEIncidentNumber = NULL, OSSCount = NULL, OSSPartialCount = NULL, ISSCount = NULL, ISSPartialCount = NULL, InternalComment = NULL, StateDistrictPAIDPerson = NULL, PASecureID = NULL, FirstName = NULL, LastName = NULL, RaceEthnicities = NULL, Genders = NULL, SendingOrCharterTypes = NULL, SendingDistrictOrCharterAUNID = NULL, AgeAtTimeOfIncident = NULL, StateGradeLevelPIMSPAID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "TempIncidentInvolvedPersonPA", objectId = TempIncidentInvolvedPersonPAID, body = list(DataObject = body), searchFields = append("TempIncidentInvolvedPersonPAID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempIncidentInvolvedPersonMNS
	#'
	#' This function returns a dataframe or json object of TempIncidentInvolvedPersonMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentInvolvedPersonMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentInvolvedPersonMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentInvolvedPersonMN') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of TempIncidentInvolvedPersonMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempIncidentInvolvedPersonMNS <- function(searchConditionsList = NULL, StateVictimTypeMNID = F, StateVictimCostMNID = F, InjuryOccured = F, WasSeriousBodilyInjury = F, ReportedToLawEnforcement = F, OffenderArrestedByLawEnforcement = F, StateOffenderActivityMNID = F, IsPhysicalAssault = F, IsPhysicalAssaultState = F, TempIncidentInvolvedPersonID = F, IncidentOffenseNameKey = F, PrimaryOffenseID = F, SecondaryOffensesBackingData = F, ExcludePrimaryOffense = F, ExistingIncidentOffenseNamesToDelete = F, InvolvementType = F, IncidentOffenseNameType = F, FreeformName = F, StudentID = F, StaffID = F, NameID = F, StaffIDDisciplineOfficer = F, PerceivedMotivationID = F, FullName = F, StudentNumber = F, PerceivedMotivationCodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OSSCount = F, OSSPartialCount = F, ISSCount = F, ISSPartialCount = F, InternalComment = F, MultipleVictimCount = F, EstimatedVictimsEnrolled = F, EstimatedVictimsNotEnrolled = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentInvolvedPersonMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempIncidentInvolvedPersonMN
	#'
	#' This function returns a dataframe or json object of a TempIncidentInvolvedPersonMN
	#' @param TempIncidentInvolvedPersonMNID The ID of the TempIncidentInvolvedPersonMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentInvolvedPersonMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentInvolvedPersonMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentInvolvedPersonMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of TempIncidentInvolvedPersonMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempIncidentInvolvedPersonMN <- function(TempIncidentInvolvedPersonMNID, StateVictimTypeMNID = F, StateVictimCostMNID = F, InjuryOccured = F, WasSeriousBodilyInjury = F, ReportedToLawEnforcement = F, OffenderArrestedByLawEnforcement = F, StateOffenderActivityMNID = F, IsPhysicalAssault = F, IsPhysicalAssaultState = F, TempIncidentInvolvedPersonID = F, IncidentOffenseNameKey = F, PrimaryOffenseID = F, SecondaryOffensesBackingData = F, ExcludePrimaryOffense = F, ExistingIncidentOffenseNamesToDelete = F, InvolvementType = F, IncidentOffenseNameType = F, FreeformName = F, StudentID = F, StaffID = F, NameID = F, StaffIDDisciplineOfficer = F, PerceivedMotivationID = F, FullName = F, StudentNumber = F, PerceivedMotivationCodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OSSCount = F, OSSPartialCount = F, ISSCount = F, ISSPartialCount = F, InternalComment = F, MultipleVictimCount = F, EstimatedVictimsEnrolled = F, EstimatedVictimsNotEnrolled = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempIncidentInvolvedPersonMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "TempIncidentInvolvedPersonMN", objectId = TempIncidentInvolvedPersonMNID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempIncidentInvolvedPersonMN
	#'
	#' This function deletes a TempIncidentInvolvedPersonMN
	#' @param TempIncidentInvolvedPersonMNID The ID of the TempIncidentInvolvedPersonMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The TempIncidentInvolvedPersonMNID of the deleted TempIncidentInvolvedPersonMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempIncidentInvolvedPersonMN <- function(TempIncidentInvolvedPersonMNID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "TempIncidentInvolvedPersonMN", objectId = TempIncidentInvolvedPersonMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempIncidentInvolvedPersonMN
	#'
	#' This function creates a TempIncidentInvolvedPersonMN
	#' @param fieldNames The field values to give the created TempIncidentInvolvedPersonMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created TempIncidentInvolvedPersonMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempIncidentInvolvedPersonMN <- function(StateVictimTypeMNID = NULL, StateVictimCostMNID = NULL, InjuryOccured = NULL, WasSeriousBodilyInjury = NULL, ReportedToLawEnforcement = NULL, OffenderArrestedByLawEnforcement = NULL, StateOffenderActivityMNID = NULL, IsPhysicalAssault = NULL, IsPhysicalAssaultState = NULL, IncidentOffenseNameKey = NULL, PrimaryOffenseID = NULL, ExcludePrimaryOffense = NULL, InvolvementType = NULL, IncidentOffenseNameType = NULL, FreeformName = NULL, StudentID = NULL, StaffID = NULL, NameID = NULL, StaffIDDisciplineOfficer = NULL, PerceivedMotivationID = NULL, FullName = NULL, StudentNumber = NULL, PerceivedMotivationCodeDescription = NULL, OSSCount = NULL, OSSPartialCount = NULL, ISSCount = NULL, ISSPartialCount = NULL, InternalComment = NULL, MultipleVictimCount = NULL, EstimatedVictimsEnrolled = NULL, EstimatedVictimsNotEnrolled = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "TempIncidentInvolvedPersonMN", body = list(DataObject = body), searchFields = append("TempIncidentInvolvedPersonMNID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempIncidentInvolvedPersonMN
	#'
	#' This function modifies a TempIncidentInvolvedPersonMN
	#' @param fieldNames The field values to give the modified TempIncidentInvolvedPersonMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified TempIncidentInvolvedPersonMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempIncidentInvolvedPersonMN <- function(TempIncidentInvolvedPersonMNID, StateVictimTypeMNID = NULL, StateVictimCostMNID = NULL, InjuryOccured = NULL, WasSeriousBodilyInjury = NULL, ReportedToLawEnforcement = NULL, OffenderArrestedByLawEnforcement = NULL, StateOffenderActivityMNID = NULL, IsPhysicalAssault = NULL, IsPhysicalAssaultState = NULL, IncidentOffenseNameKey = NULL, PrimaryOffenseID = NULL, ExcludePrimaryOffense = NULL, InvolvementType = NULL, IncidentOffenseNameType = NULL, FreeformName = NULL, StudentID = NULL, StaffID = NULL, NameID = NULL, StaffIDDisciplineOfficer = NULL, PerceivedMotivationID = NULL, FullName = NULL, StudentNumber = NULL, PerceivedMotivationCodeDescription = NULL, OSSCount = NULL, OSSPartialCount = NULL, ISSCount = NULL, ISSPartialCount = NULL, InternalComment = NULL, MultipleVictimCount = NULL, EstimatedVictimsEnrolled = NULL, EstimatedVictimsNotEnrolled = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "TempIncidentInvolvedPersonMN", objectId = TempIncidentInvolvedPersonMNID, body = list(DataObject = body), searchFields = append("TempIncidentInvolvedPersonMNID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempIncidentInvolvedPersonINS
	#'
	#' This function returns a dataframe or json object of TempIncidentInvolvedPersonINS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentInvolvedPersonINS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentInvolvedPersonINS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentInvolvedPersonIN') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of TempIncidentInvolvedPersonINS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempIncidentInvolvedPersonINS <- function(searchConditionsList = NULL, StateCriminalEventINID = F, StateArrestTypeINID = F, StateArrestReasonINID = F, IsChemicalRestraint = F, IsMechanicalRestraint = F, IsPhysicalRestraint = F, IsSeclusion = F, StaffIDResourceOfficer = F, TempIncidentInvolvedPersonID = F, IncidentOffenseNameKey = F, PrimaryOffenseID = F, SecondaryOffensesBackingData = F, ExcludePrimaryOffense = F, ExistingIncidentOffenseNamesToDelete = F, InvolvementType = F, IncidentOffenseNameType = F, FreeformName = F, StudentID = F, StaffID = F, NameID = F, StaffIDDisciplineOfficer = F, PerceivedMotivationID = F, FullName = F, StudentNumber = F, PerceivedMotivationCodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OSSCount = F, OSSPartialCount = F, ISSCount = F, ISSPartialCount = F, InternalComment = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentInvolvedPersonIN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempIncidentInvolvedPersonIN
	#'
	#' This function returns a dataframe or json object of a TempIncidentInvolvedPersonIN
	#' @param TempIncidentInvolvedPersonINID The ID of the TempIncidentInvolvedPersonIN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentInvolvedPersonIN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentInvolvedPersonIN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentInvolvedPersonIN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of TempIncidentInvolvedPersonIN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempIncidentInvolvedPersonIN <- function(TempIncidentInvolvedPersonINID, StateCriminalEventINID = F, StateArrestTypeINID = F, StateArrestReasonINID = F, IsChemicalRestraint = F, IsMechanicalRestraint = F, IsPhysicalRestraint = F, IsSeclusion = F, StaffIDResourceOfficer = F, TempIncidentInvolvedPersonID = F, IncidentOffenseNameKey = F, PrimaryOffenseID = F, SecondaryOffensesBackingData = F, ExcludePrimaryOffense = F, ExistingIncidentOffenseNamesToDelete = F, InvolvementType = F, IncidentOffenseNameType = F, FreeformName = F, StudentID = F, StaffID = F, NameID = F, StaffIDDisciplineOfficer = F, PerceivedMotivationID = F, FullName = F, StudentNumber = F, PerceivedMotivationCodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OSSCount = F, OSSPartialCount = F, ISSCount = F, ISSPartialCount = F, InternalComment = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempIncidentInvolvedPersonINID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "TempIncidentInvolvedPersonIN", objectId = TempIncidentInvolvedPersonINID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempIncidentInvolvedPersonIN
	#'
	#' This function deletes a TempIncidentInvolvedPersonIN
	#' @param TempIncidentInvolvedPersonINID The ID of the TempIncidentInvolvedPersonIN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The TempIncidentInvolvedPersonINID of the deleted TempIncidentInvolvedPersonIN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempIncidentInvolvedPersonIN <- function(TempIncidentInvolvedPersonINID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "TempIncidentInvolvedPersonIN", objectId = TempIncidentInvolvedPersonINID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempIncidentInvolvedPersonIN
	#'
	#' This function creates a TempIncidentInvolvedPersonIN
	#' @param fieldNames The field values to give the created TempIncidentInvolvedPersonIN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created TempIncidentInvolvedPersonIN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempIncidentInvolvedPersonIN <- function(StateCriminalEventINID = NULL, StateArrestTypeINID = NULL, StateArrestReasonINID = NULL, IsChemicalRestraint = NULL, IsMechanicalRestraint = NULL, IsPhysicalRestraint = NULL, IsSeclusion = NULL, StaffIDResourceOfficer = NULL, IncidentOffenseNameKey = NULL, PrimaryOffenseID = NULL, ExcludePrimaryOffense = NULL, InvolvementType = NULL, IncidentOffenseNameType = NULL, FreeformName = NULL, StudentID = NULL, StaffID = NULL, NameID = NULL, StaffIDDisciplineOfficer = NULL, PerceivedMotivationID = NULL, FullName = NULL, StudentNumber = NULL, PerceivedMotivationCodeDescription = NULL, OSSCount = NULL, OSSPartialCount = NULL, ISSCount = NULL, ISSPartialCount = NULL, InternalComment = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "TempIncidentInvolvedPersonIN", body = list(DataObject = body), searchFields = append("TempIncidentInvolvedPersonINID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempIncidentInvolvedPersonIN
	#'
	#' This function modifies a TempIncidentInvolvedPersonIN
	#' @param fieldNames The field values to give the modified TempIncidentInvolvedPersonIN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified TempIncidentInvolvedPersonIN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempIncidentInvolvedPersonIN <- function(TempIncidentInvolvedPersonINID, StateCriminalEventINID = NULL, StateArrestTypeINID = NULL, StateArrestReasonINID = NULL, IsChemicalRestraint = NULL, IsMechanicalRestraint = NULL, IsPhysicalRestraint = NULL, IsSeclusion = NULL, StaffIDResourceOfficer = NULL, IncidentOffenseNameKey = NULL, PrimaryOffenseID = NULL, ExcludePrimaryOffense = NULL, InvolvementType = NULL, IncidentOffenseNameType = NULL, FreeformName = NULL, StudentID = NULL, StaffID = NULL, NameID = NULL, StaffIDDisciplineOfficer = NULL, PerceivedMotivationID = NULL, FullName = NULL, StudentNumber = NULL, PerceivedMotivationCodeDescription = NULL, OSSCount = NULL, OSSPartialCount = NULL, ISSCount = NULL, ISSPartialCount = NULL, InternalComment = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "TempIncidentInvolvedPersonIN", objectId = TempIncidentInvolvedPersonINID, body = list(DataObject = body), searchFields = append("TempIncidentInvolvedPersonINID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempIncidentInvolvedPeople
	#'
	#' This function returns a dataframe or json object of TempIncidentInvolvedPeople
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentInvolvedPeople. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentInvolvedPeople.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentInvolvedPerson') to get more field paths.
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
	#' @concept Discipline
	#' @return A list of TempIncidentInvolvedPeople
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempIncidentInvolvedPeople <- function(searchConditionsList = NULL, TempIncidentInvolvedPersonID = F, IncidentOffenseNameKey = F, PrimaryOffenseID = F, SecondaryOffensesBackingData = F, ExcludePrimaryOffense = F, ExistingIncidentOffenseNamesToDelete = F, InvolvementType = F, IncidentOffenseNameType = F, FreeformName = F, StudentID = F, StaffID = F, NameID = F, StaffIDDisciplineOfficer = F, PerceivedMotivationID = F, FullName = F, StudentNumber = F, PerceivedMotivationCodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OSSCount = F, OSSPartialCount = F, ISSCount = F, ISSPartialCount = F, InternalComment = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentInvolvedPerson", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempIncidentInvolvedPerson
	#'
	#' This function returns a dataframe or json object of a TempIncidentInvolvedPerson
	#' @param TempIncidentInvolvedPersonID The ID of the TempIncidentInvolvedPerson to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempIncidentInvolvedPerson. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempIncidentInvolvedPerson.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempIncidentInvolvedPerson') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of TempIncidentInvolvedPerson
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempIncidentInvolvedPerson <- function(TempIncidentInvolvedPersonID, IncidentOffenseNameKey = F, PrimaryOffenseID = F, SecondaryOffensesBackingData = F, ExcludePrimaryOffense = F, ExistingIncidentOffenseNamesToDelete = F, InvolvementType = F, IncidentOffenseNameType = F, FreeformName = F, StudentID = F, StaffID = F, NameID = F, StaffIDDisciplineOfficer = F, PerceivedMotivationID = F, FullName = F, StudentNumber = F, PerceivedMotivationCodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OSSCount = F, OSSPartialCount = F, ISSCount = F, ISSPartialCount = F, InternalComment = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempIncidentInvolvedPersonID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "TempIncidentInvolvedPerson", objectId = TempIncidentInvolvedPersonID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempIncidentInvolvedPerson
	#'
	#' This function deletes a TempIncidentInvolvedPerson
	#' @param TempIncidentInvolvedPersonID The ID of the TempIncidentInvolvedPerson to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The TempIncidentInvolvedPersonID of the deleted TempIncidentInvolvedPerson.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempIncidentInvolvedPerson <- function(TempIncidentInvolvedPersonID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Discipline", objectName = "TempIncidentInvolvedPerson", objectId = TempIncidentInvolvedPersonID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempIncidentInvolvedPerson
	#'
	#' This function creates a TempIncidentInvolvedPerson
	#' @param fieldNames The field values to give the created TempIncidentInvolvedPerson. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A newly created TempIncidentInvolvedPerson
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempIncidentInvolvedPerson <- function(IncidentOffenseNameKey = NULL, PrimaryOffenseID = NULL, ExcludePrimaryOffense = NULL, InvolvementType = NULL, IncidentOffenseNameType = NULL, FreeformName = NULL, StudentID = NULL, StaffID = NULL, NameID = NULL, StaffIDDisciplineOfficer = NULL, PerceivedMotivationID = NULL, FullName = NULL, StudentNumber = NULL, PerceivedMotivationCodeDescription = NULL, OSSCount = NULL, OSSPartialCount = NULL, ISSCount = NULL, ISSPartialCount = NULL, InternalComment = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Discipline", objectName = "TempIncidentInvolvedPerson", body = list(DataObject = body), searchFields = append("TempIncidentInvolvedPersonID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempIncidentInvolvedPerson
	#'
	#' This function modifies a TempIncidentInvolvedPerson
	#' @param fieldNames The field values to give the modified TempIncidentInvolvedPerson. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return The modified TempIncidentInvolvedPerson
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempIncidentInvolvedPerson <- function(TempIncidentInvolvedPersonID, IncidentOffenseNameKey = NULL, PrimaryOffenseID = NULL, ExcludePrimaryOffense = NULL, InvolvementType = NULL, IncidentOffenseNameType = NULL, FreeformName = NULL, StudentID = NULL, StaffID = NULL, NameID = NULL, StaffIDDisciplineOfficer = NULL, PerceivedMotivationID = NULL, FullName = NULL, StudentNumber = NULL, PerceivedMotivationCodeDescription = NULL, OSSCount = NULL, OSSPartialCount = NULL, ISSCount = NULL, ISSPartialCount = NULL, InternalComment = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Discipline", objectName = "TempIncidentInvolvedPerson", objectId = TempIncidentInvolvedPersonID, body = list(DataObject = body), searchFields = append("TempIncidentInvolvedPersonID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}
