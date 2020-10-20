
	#' List ActionAttendanceTypes
	#'
	#' This function returns a dataframe or json object of ActionAttendanceTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('ActionAttendanceType') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of ActionAttendanceTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listActionAttendanceTypes <- function(searchConditionsList = NULL, ActionAttendanceTypeID = F, ActionID = F, AttendanceTypeID = F, EntityGroupKey = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, ActionAttendanceTypeIDClonedFrom = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "ActionAttendanceType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get ActionAttendanceType
	#'
	#' This function returns a dataframe or json object of an ActionAttendanceType
	#' @param ActionAttendanceTypeID The ID of the ActionAttendanceType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('ActionAttendanceType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getActionAttendanceType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getActionAttendanceType <- function(ActionAttendanceTypeID, ActionID = F, AttendanceTypeID = F, EntityGroupKey = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, ActionAttendanceTypeIDClonedFrom = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ActionAttendanceTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "ActionAttendanceType", objectId = ActionAttendanceTypeID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List IncidentOffenseNameActionDetailPeriods
	#'
	#' This function returns a dataframe or json object of IncidentOffenseNameActionDetailPeriods
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('IncidentOffenseNameActionDetailPeriod') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of IncidentOffenseNameActionDetailPeriods
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listIncidentOffenseNameActionDetailPeriods <- function(searchConditionsList = NULL, IncidentOffenseNameActionDetailPeriodID = F, IncidentOffenseNameActionDetailID = F, AttendancePeriodID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "IncidentOffenseNameActionDetailPeriod", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get IncidentOffenseNameActionDetailPeriod
	#'
	#' This function returns a dataframe or json object of an IncidentOffenseNameActionDetailPeriod
	#' @param IncidentOffenseNameActionDetailPeriodID The ID of the IncidentOffenseNameActionDetailPeriod to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('IncidentOffenseNameActionDetailPeriod') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getIncidentOffenseNameActionDetailPeriod
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getIncidentOffenseNameActionDetailPeriod <- function(IncidentOffenseNameActionDetailPeriodID, IncidentOffenseNameActionDetailID = F, AttendancePeriodID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "IncidentOffenseNameActionDetailPeriodID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "IncidentOffenseNameActionDetailPeriod", objectId = IncidentOffenseNameActionDetailPeriodID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DisciplineConfigEntityGroupYears
	#'
	#' This function returns a dataframe or json object of DisciplineConfigEntityGroupYears
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('DisciplineConfigEntityGroupYear') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of DisciplineConfigEntityGroupYears
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDisciplineConfigEntityGroupYears <- function(searchConditionsList = NULL, ConfigEntityGroupYearID = F, EntityID = F, EntityGroupKey = F, SchoolYearID = F, UseAlternateActionDetails = F, DetentionsOnMon = F, DetentionsOnTue = F, DetentionsOnWed = F, DetentionsOnThu = F, DetentionsOnFri = F, DetentionsOnSat = F, DetentionsOnSun = F, InSchoolSuspensionsOnMon = F, InSchoolSuspensionsOnTue = F, InSchoolSuspensionsOnWed = F, InSchoolSuspensionsOnThu = F, InSchoolSuspensionsOnFri = F, InSchoolSuspensionsOnSat = F, InSchoolSuspensionsOnSun = F, OutOfSchoolSuspensionsOnMon = F, OutOfSchoolSuspensionsOnTue = F, OutOfSchoolSuspensionsOnWed = F, OutOfSchoolSuspensionsOnThu = F, OutOfSchoolSuspensionsOnFri = F, OutOfSchoolSuspensionsOnSat = F, OutOfSchoolSuspensionsOnSun = F, ExpulsionsOnMon = F, ExpulsionsOnTue = F, ExpulsionsOnWed = F, ExpulsionsOnThu = F, ExpulsionsOnFri = F, ExpulsionsOnSat = F, ExpulsionsOnSun = F, DefaultActionStatusCode = F, TardyKioskDisciplineSlipTitle = F, IncludeStudentNameAndOrNumberOnLetter = F, IncludeGradeLevelOnLetter = F, IncludeSchoolOrCampusOnLetter = F, IncludeParentNameAndOrPhoneNumberOnLetter = F, IncludeDisciplinaryActionAndDetailsOnLetter = F, IncludeSignatureLineForParentOnLetter = F, IncludeSignatureLineForStudentOnLetter = F, IncludeSignatureLineForOfficeOnLetter = F, ActionStatusDefaultValue = F, ConfigEntityGroupYearIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, IncludeOffenseDescriptionOnLetter = F, IncludeIncidentDateAndTimeOnLetter = F, IncludeIncidentDescriptionOnLetter = F, IncludeReferredByOnLetter = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "ConfigEntityGroupYear", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get DisciplineConfigEntityGroupYear
	#'
	#' This function returns a dataframe or json object of a DisciplineConfigEntityGroupYear
	#' @param DisciplineConfigEntityGroupYearID The ID of the DisciplineConfigEntityGroupYear to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('DisciplineConfigEntityGroupYear') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getDisciplineConfigEntityGroupYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDisciplineConfigEntityGroupYear <- function(DisciplineConfigEntityGroupYearID, ConfigEntityGroupYearID = F, EntityID = F, EntityGroupKey = F, SchoolYearID = F, UseAlternateActionDetails = F, DetentionsOnMon = F, DetentionsOnTue = F, DetentionsOnWed = F, DetentionsOnThu = F, DetentionsOnFri = F, DetentionsOnSat = F, DetentionsOnSun = F, InSchoolSuspensionsOnMon = F, InSchoolSuspensionsOnTue = F, InSchoolSuspensionsOnWed = F, InSchoolSuspensionsOnThu = F, InSchoolSuspensionsOnFri = F, InSchoolSuspensionsOnSat = F, InSchoolSuspensionsOnSun = F, OutOfSchoolSuspensionsOnMon = F, OutOfSchoolSuspensionsOnTue = F, OutOfSchoolSuspensionsOnWed = F, OutOfSchoolSuspensionsOnThu = F, OutOfSchoolSuspensionsOnFri = F, OutOfSchoolSuspensionsOnSat = F, OutOfSchoolSuspensionsOnSun = F, ExpulsionsOnMon = F, ExpulsionsOnTue = F, ExpulsionsOnWed = F, ExpulsionsOnThu = F, ExpulsionsOnFri = F, ExpulsionsOnSat = F, ExpulsionsOnSun = F, DefaultActionStatusCode = F, TardyKioskDisciplineSlipTitle = F, IncludeStudentNameAndOrNumberOnLetter = F, IncludeGradeLevelOnLetter = F, IncludeSchoolOrCampusOnLetter = F, IncludeParentNameAndOrPhoneNumberOnLetter = F, IncludeDisciplinaryActionAndDetailsOnLetter = F, IncludeSignatureLineForParentOnLetter = F, IncludeSignatureLineForStudentOnLetter = F, IncludeSignatureLineForOfficeOnLetter = F, ActionStatusDefaultValue = F, ConfigEntityGroupYearIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, IncludeOffenseDescriptionOnLetter = F, IncludeIncidentDateAndTimeOnLetter = F, IncludeIncidentDescriptionOnLetter = F, IncludeReferredByOnLetter = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DisciplineConfigEntityGroupYearID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "ConfigEntityGroupYear", objectId = DisciplineConfigEntityGroupYearID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempIncidentOffenseNameActionDetails
	#'
	#' This function returns a dataframe or json object of TempIncidentOffenseNameActionDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempIncidentOffenseNameActionDetail') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of TempIncidentOffenseNameActionDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempIncidentOffenseNameActionDetails <- function(searchConditionsList = NULL, TempIncidentOffenseNameActionDetailID = F, IncidentOffenseNameActionDetailID = F, FullName = F, InvolvementType = F, OffenseCodeDescription = F, IsPrimaryOffense = F, ActionCodeDescription = F, DurationToServe = F, DurationType = F, CreateAttendance = F, IsAlternate = F, LocationID = F, PartialDayPeriods = F, OldStatus = F, NewStatus = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TempIncidentOffenseNameActionID = F, ScheduledTime = F, DurationServed = F, StaffIDFollowUpOfficer = F, Status = F, IsGuardianNotified = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffenseNameActionDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempIncidentOffenseNameActionDetail
	#'
	#' This function returns a dataframe or json object of a TempIncidentOffenseNameActionDetail
	#' @param TempIncidentOffenseNameActionDetailID The ID of the TempIncidentOffenseNameActionDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempIncidentOffenseNameActionDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getTempIncidentOffenseNameActionDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempIncidentOffenseNameActionDetail <- function(TempIncidentOffenseNameActionDetailID, IncidentOffenseNameActionDetailID = F, FullName = F, InvolvementType = F, OffenseCodeDescription = F, IsPrimaryOffense = F, ActionCodeDescription = F, DurationToServe = F, DurationType = F, CreateAttendance = F, IsAlternate = F, LocationID = F, PartialDayPeriods = F, OldStatus = F, NewStatus = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TempIncidentOffenseNameActionID = F, ScheduledTime = F, DurationServed = F, StaffIDFollowUpOfficer = F, Status = F, IsGuardianNotified = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempIncidentOffenseNameActionDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameActionDetail", objectId = TempIncidentOffenseNameActionDetailID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempIncidentOffenseNameActions
	#'
	#' This function returns a dataframe or json object of TempIncidentOffenseNameActions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempIncidentOffenseNameAction') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of TempIncidentOffenseNameActions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempIncidentOffenseNameActions <- function(searchConditionsList = NULL, FullName = F, InvolvementType = F, OffenseCodeDescription = F, IsPrimaryOffense = F, ActionCodeDescription = F, DurationToServe = F, StaffIDAuthorizedByName = F, PerceivedMotivationCodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Comment = F, StudentNumber = F, TempIncidentOffenseNameActionID = F, EntityID = F, ActionTypeID = F, ActionTypeCode = F, ActionID = F, Status = F, OrderedDate = F, DurationType = F, StaffIDAuthorizedBy = F, StaffIDFollowUpOfficer = F, LocationID = F, BuildingID = F, RoomID = F, TempIncidentOffenseNameID = F, IsGuardianNotified = F, StartTime = F, InternalComment = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffenseNameAction", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempIncidentOffenseNameAction
	#'
	#' This function returns a dataframe or json object of a TempIncidentOffenseNameAction
	#' @param TempIncidentOffenseNameActionID The ID of the TempIncidentOffenseNameAction to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempIncidentOffenseNameAction') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getTempIncidentOffenseNameAction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempIncidentOffenseNameAction <- function(TempIncidentOffenseNameActionID, FullName = F, InvolvementType = F, OffenseCodeDescription = F, IsPrimaryOffense = F, ActionCodeDescription = F, DurationToServe = F, StaffIDAuthorizedByName = F, PerceivedMotivationCodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Comment = F, StudentNumber = F, EntityID = F, ActionTypeID = F, ActionTypeCode = F, ActionID = F, Status = F, OrderedDate = F, DurationType = F, StaffIDAuthorizedBy = F, StaffIDFollowUpOfficer = F, LocationID = F, BuildingID = F, RoomID = F, TempIncidentOffenseNameID = F, IsGuardianNotified = F, StartTime = F, InternalComment = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempIncidentOffenseNameActionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameAction", objectId = TempIncidentOffenseNameActionID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Incidents
	#'
	#' This function returns a dataframe or json object of Incidents
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('Incident') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of Incidents
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listIncidents <- function(searchConditionsList = NULL, IncidentID = F, DistrictID = F, EntityID = F, SchoolYearID = F, IncidentNumber = F, DateTime = F, Type = F, LocationID = F, BuildingID = F, RoomID = F, Description = F, DamageCost = F, ActionIDRecommended = F, ReferredByType = F, ReferredByNameID = F, ReferredByFreeformName = F, ReferredByName = F, IncidentNumberValue = F, HasActions = F, HasActionsForOffenders = F, HasOpenActions = F, HasOverdueActionDetails = F, IsReferralOrWarning = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IncidentMNID = F, StateDIRSTimeMNID = F, ReportedToLawEnforcement = F, NumberOfNonEnrolledOffenders = F, NumberOfNonEnrolledVictims = F, IsIncidentOrWarning = F, IsSuppressed = F, HasWeapons = F, HasDrugs = F, IsReadOnlyHistoricalRecord = F, DateBeforeLastEffectiveRun = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "Incident", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get Incident
	#'
	#' This function returns a dataframe or json object of an Incident
	#' @param IncidentID The ID of the Incident to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('Incident') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getIncident
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getIncident <- function(IncidentID, DistrictID = F, EntityID = F, SchoolYearID = F, IncidentNumber = F, DateTime = F, Type = F, LocationID = F, BuildingID = F, RoomID = F, Description = F, DamageCost = F, ActionIDRecommended = F, ReferredByType = F, ReferredByNameID = F, ReferredByFreeformName = F, ReferredByName = F, IncidentNumberValue = F, HasActions = F, HasActionsForOffenders = F, HasOpenActions = F, HasOverdueActionDetails = F, IsReferralOrWarning = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IncidentMNID = F, StateDIRSTimeMNID = F, ReportedToLawEnforcement = F, NumberOfNonEnrolledOffenders = F, NumberOfNonEnrolledVictims = F, IsIncidentOrWarning = F, IsSuppressed = F, HasWeapons = F, HasDrugs = F, IsReadOnlyHistoricalRecord = F, DateBeforeLastEffectiveRun = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "IncidentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "Incident", objectId = IncidentID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List IncidentOffenses
	#'
	#' This function returns a dataframe or json object of IncidentOffenses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('IncidentOffense') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of IncidentOffenses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listIncidentOffenses <- function(searchConditionsList = NULL, IncidentOffenseID = F, IncidentID = F, OffenseID = F, IsPrimaryOffense = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasActions = F, HasWeapons = F, HasDrugs = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "IncidentOffense", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get IncidentOffense
	#'
	#' This function returns a dataframe or json object of an IncidentOffense
	#' @param IncidentOffenseID The ID of the IncidentOffense to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('IncidentOffense') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getIncidentOffense
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getIncidentOffense <- function(IncidentOffenseID, IncidentID = F, OffenseID = F, IsPrimaryOffense = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasActions = F, HasWeapons = F, HasDrugs = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "IncidentOffenseID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "IncidentOffense", objectId = IncidentOffenseID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List IncidentOffenseNames
	#'
	#' This function returns a dataframe or json object of IncidentOffenseNames
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('IncidentOffenseName') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of IncidentOffenseNames
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listIncidentOffenseNames <- function(searchConditionsList = NULL, IncidentOffenseNameID = F, IncidentOffenseID = F, NameID = F, IncidentOffenseNameType = F, FreeformName = F, InvolvementType = F, StaffIDDisciplineOfficer = F, Statement = F, IsGuardianNotified = F, PerceivedMotivationID = F, OffenseLevelID = F, PersonalName = F, HasActions = F, HasOpenActions = F, HasOverdueActionDetails = F, IsStudentOffender = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IncidentOffenseNameMNID = F, StateVictimTypeMNID = F, StateVictimCostMNID = F, InjuryOccured = F, WasSeriousBodilyInjury = F, AttachmentCount = F, HasWeapons = F, HasDrugs = F, ReportedToLawEnforcement = F, StateOffenderActivityMNID = F, HasDangerousWeapons = F, IsPhysicalAssault = F, IsPhysicalAssaultState = F, FirstDrugCodeforNorthEastExport = F, OffenderArrestedByLawEnforcement = F, IsReadOnlyHistoricalRecord = F, DisciplineThresholdID = F, InternalComment = F, MultipleVictimCount = F, EstimatedVictimsEnrolled = F, EstimatedVictimsNotEnrolled = F, CreatedBeforeLastEffectiveRunAndWasWithinDelay = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "IncidentOffenseName", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get IncidentOffenseName
	#'
	#' This function returns a dataframe or json object of an IncidentOffenseName
	#' @param IncidentOffenseNameID The ID of the IncidentOffenseName to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('IncidentOffenseName') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getIncidentOffenseName
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getIncidentOffenseName <- function(IncidentOffenseNameID, IncidentOffenseID = F, NameID = F, IncidentOffenseNameType = F, FreeformName = F, InvolvementType = F, StaffIDDisciplineOfficer = F, Statement = F, IsGuardianNotified = F, PerceivedMotivationID = F, OffenseLevelID = F, PersonalName = F, HasActions = F, HasOpenActions = F, HasOverdueActionDetails = F, IsStudentOffender = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IncidentOffenseNameMNID = F, StateVictimTypeMNID = F, StateVictimCostMNID = F, InjuryOccured = F, WasSeriousBodilyInjury = F, AttachmentCount = F, HasWeapons = F, HasDrugs = F, ReportedToLawEnforcement = F, StateOffenderActivityMNID = F, HasDangerousWeapons = F, IsPhysicalAssault = F, IsPhysicalAssaultState = F, FirstDrugCodeforNorthEastExport = F, OffenderArrestedByLawEnforcement = F, IsReadOnlyHistoricalRecord = F, DisciplineThresholdID = F, InternalComment = F, MultipleVictimCount = F, EstimatedVictimsEnrolled = F, EstimatedVictimsNotEnrolled = F, CreatedBeforeLastEffectiveRunAndWasWithinDelay = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "IncidentOffenseNameID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "IncidentOffenseName", objectId = IncidentOffenseNameID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List IncidentOffenseNameActions
	#'
	#' This function returns a dataframe or json object of IncidentOffenseNameActions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('IncidentOffenseNameAction') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of IncidentOffenseNameActions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listIncidentOffenseNameActions <- function(searchConditionsList = NULL, IncidentOffenseNameActionID = F, IncidentOffenseNameID = F, ActionID = F, Status = F, OrderedDate = F, DurationToServe = F, EntityID = F, LocationID = F, BuildingID = F, RoomID = F, StaffIDAuthorizedBy = F, StaffIDFollowUpOfficer = F, IsGuardianNotified = F, Comment = F, DurationServedOverride = F, DurationServed = F, ActualDurationServed = F, TotalDurationServed = F, DurationToServeWithLabel = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IncidentOffenseNameActionMNID = F, StateDIRSAESTypeMNID = F, DateExpulsionExclusionEnds = F, ReturnBeforeYearEnd = F, ExclusionThroughYearEnd = F, ExpulsionModified = F, ExpulsionThroughYearEnd = F, DIRSActionExplanation = F, FirstActionDetailDateNorthEastExport = F, LastActionDetailDateNorthEastExport = F, ActionTypeID = F, DurationType = F, StartTime = F, IsReadOnlyHistoricalRecord = F, InternalComment = F, NoServiceProvidedExplanation = F, MeetsLastRunDates = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "IncidentOffenseNameAction", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get IncidentOffenseNameAction
	#'
	#' This function returns a dataframe or json object of an IncidentOffenseNameAction
	#' @param IncidentOffenseNameActionID The ID of the IncidentOffenseNameAction to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('IncidentOffenseNameAction') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getIncidentOffenseNameAction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getIncidentOffenseNameAction <- function(IncidentOffenseNameActionID, IncidentOffenseNameID = F, ActionID = F, Status = F, OrderedDate = F, DurationToServe = F, EntityID = F, LocationID = F, BuildingID = F, RoomID = F, StaffIDAuthorizedBy = F, StaffIDFollowUpOfficer = F, IsGuardianNotified = F, Comment = F, DurationServedOverride = F, DurationServed = F, ActualDurationServed = F, TotalDurationServed = F, DurationToServeWithLabel = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IncidentOffenseNameActionMNID = F, StateDIRSAESTypeMNID = F, DateExpulsionExclusionEnds = F, ReturnBeforeYearEnd = F, ExclusionThroughYearEnd = F, ExpulsionModified = F, ExpulsionThroughYearEnd = F, DIRSActionExplanation = F, FirstActionDetailDateNorthEastExport = F, LastActionDetailDateNorthEastExport = F, ActionTypeID = F, DurationType = F, StartTime = F, IsReadOnlyHistoricalRecord = F, InternalComment = F, NoServiceProvidedExplanation = F, MeetsLastRunDates = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "IncidentOffenseNameActionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "IncidentOffenseNameAction", objectId = IncidentOffenseNameActionID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List IncidentOffenseNameActionDetails
	#'
	#' This function returns a dataframe or json object of IncidentOffenseNameActionDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('IncidentOffenseNameActionDetail') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of IncidentOffenseNameActionDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listIncidentOffenseNameActionDetails <- function(searchConditionsList = NULL, IncidentOffenseNameActionDetailID = F, IncidentOffenseNameActionID = F, Status = F, ScheduledTime = F, IsAlternate = F, DurationToServe = F, DurationServed = F, LocationID = F, BuildingID = F, RoomID = F, StaffIDFollowUpOfficer = F, IsGuardianNotified = F, Comment = F, LastAlternate = F, RenderReissueOption = F, DurationToServeWithLabel = F, DurationServedWithLabel = F, Overdue = F, PartialDayPeriods = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, InternalComment = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "IncidentOffenseNameActionDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get IncidentOffenseNameActionDetail
	#'
	#' This function returns a dataframe or json object of an IncidentOffenseNameActionDetail
	#' @param IncidentOffenseNameActionDetailID The ID of the IncidentOffenseNameActionDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('IncidentOffenseNameActionDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getIncidentOffenseNameActionDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getIncidentOffenseNameActionDetail <- function(IncidentOffenseNameActionDetailID, IncidentOffenseNameActionID = F, Status = F, ScheduledTime = F, IsAlternate = F, DurationToServe = F, DurationServed = F, LocationID = F, BuildingID = F, RoomID = F, StaffIDFollowUpOfficer = F, IsGuardianNotified = F, Comment = F, LastAlternate = F, RenderReissueOption = F, DurationToServeWithLabel = F, DurationServedWithLabel = F, Overdue = F, PartialDayPeriods = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, InternalComment = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "IncidentOffenseNameActionDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "IncidentOffenseNameActionDetail", objectId = IncidentOffenseNameActionDetailID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List NextIncidentNumbers
	#'
	#' This function returns a dataframe or json object of NextIncidentNumbers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('NextIncidentNumber') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of NextIncidentNumbers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNextIncidentNumbers <- function(searchConditionsList = NULL, NextIncidentNumberID = F, DistrictID = F, SchoolYearID = F, SequenceNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "NextIncidentNumber", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get NextIncidentNumber
	#'
	#' This function returns a dataframe or json object of a NextIncidentNumber
	#' @param NextIncidentNumberID The ID of the NextIncidentNumber to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('NextIncidentNumber') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getNextIncidentNumber
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNextIncidentNumber <- function(NextIncidentNumberID, DistrictID = F, SchoolYearID = F, SequenceNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NextIncidentNumberID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "NextIncidentNumber", objectId = NextIncidentNumberID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempIncidentOffenseNameActionDetailRecords
	#'
	#' This function returns a dataframe or json object of TempIncidentOffenseNameActionDetailRecords
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempIncidentOffenseNameActionDetailRecord') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of TempIncidentOffenseNameActionDetailRecords
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempIncidentOffenseNameActionDetailRecords <- function(searchConditionsList = NULL, TempIncidentOffenseNameActionDetailRecordID = F, IncidentOffenseNameActionID = F, StatusCode = F, Status = F, ScheduledTime = F, IsAlternate = F, DurationToServe = F, DurationServed = F, DurationType = F, LocationID = F, Location = F, BuildingID = F, Building = F, RoomID = F, RoomNumber = F, StaffIDFollowUpOfficer = F, StaffFollowUpOfficerName = F, IsGuardianNotified = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffenseNameActionDetailRecord", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempIncidentOffenseNameActionDetailRecord
	#'
	#' This function returns a dataframe or json object of a TempIncidentOffenseNameActionDetailRecord
	#' @param TempIncidentOffenseNameActionDetailRecordID The ID of the TempIncidentOffenseNameActionDetailRecord to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempIncidentOffenseNameActionDetailRecord') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getTempIncidentOffenseNameActionDetailRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempIncidentOffenseNameActionDetailRecord <- function(TempIncidentOffenseNameActionDetailRecordID, IncidentOffenseNameActionID = F, StatusCode = F, Status = F, ScheduledTime = F, IsAlternate = F, DurationToServe = F, DurationServed = F, DurationType = F, LocationID = F, Location = F, BuildingID = F, Building = F, RoomID = F, RoomNumber = F, StaffIDFollowUpOfficer = F, StaffFollowUpOfficerName = F, IsGuardianNotified = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempIncidentOffenseNameActionDetailRecordID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameActionDetailRecord", objectId = TempIncidentOffenseNameActionDetailRecordID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DisciplineActions
	#'
	#' This function returns a dataframe or json object of DisciplineActions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('DisciplineAction') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of DisciplineActions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDisciplineActions <- function(searchConditionsList = NULL, ActionID = F, DistrictID = F, SchoolYearID = F, Code = F, Description = F, DurationType = F, DefaultDuration = F, DefaultLocationID = F, SuppressCreationOfActionDetails = F, CodeDescription = F, CreateAttendanceForActionDetail = F, ActionIDClonedFrom = F, FederalDisciplineCategoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ActionMNID = F, StateActionTypeMNID = F, ActionTypeID = F, RestraintSeclusion = F, TransferToAlternativeSchool = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "Action", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get DisciplineAction
	#'
	#' This function returns a dataframe or json object of a DisciplineAction
	#' @param DisciplineActionID The ID of the DisciplineAction to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('DisciplineAction') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getDisciplineAction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDisciplineAction <- function(DisciplineActionID, ActionID = F, DistrictID = F, SchoolYearID = F, Code = F, Description = F, DurationType = F, DefaultDuration = F, DefaultLocationID = F, SuppressCreationOfActionDetails = F, CodeDescription = F, CreateAttendanceForActionDetail = F, ActionIDClonedFrom = F, FederalDisciplineCategoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ActionMNID = F, StateActionTypeMNID = F, ActionTypeID = F, RestraintSeclusion = F, TransferToAlternativeSchool = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DisciplineActionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "Action", objectId = DisciplineActionID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DisciplineConfigDistrictYears
	#'
	#' This function returns a dataframe or json object of DisciplineConfigDistrictYears
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('DisciplineConfigDistrictYear') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of DisciplineConfigDistrictYears
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDisciplineConfigDistrictYears <- function(searchConditionsList = NULL, ConfigDistrictYearID = F, DistrictID = F, SchoolYearID = F, RestartIncidentNumberThisYear = F, AllowOnlyOneOffensePerIncident = F, DefaultDisciplineScreenDateAndTimes = F, DefaultOffenseValueFromPreviousPerson = F, DefaultActionValueFromPreviousPerson = F, UsePerceivedMotivation = F, UseIncidentBuildingAndRoom = F, ConfigDistrictYearIDClonedFrom = F, AllowUseOfWarning = F, AllowActionRecommendationsOnReferrals = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DisplayWarningsInFamilyAndStudentAccess = F, DaysToDelayDisplayOfIncidentsInFamilyAndStudentAccess = F, AllowActionTypeUpdate = F, DisplayStudentOffensesForAllEntities = F, DisplayInvolvedPersonsFromAllEntities = F, AllowDurationTypeUpdate = F, DefaultActionStatus = F, DefaultGuardianNotifiedOnActionDetailFromAction = F, IsReadOnlyHistoricalRecord = F, AllowInternalComments = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "ConfigDistrictYear", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get DisciplineConfigDistrictYear
	#'
	#' This function returns a dataframe or json object of a DisciplineConfigDistrictYear
	#' @param DisciplineConfigDistrictYearID The ID of the DisciplineConfigDistrictYear to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('DisciplineConfigDistrictYear') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getDisciplineConfigDistrictYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDisciplineConfigDistrictYear <- function(DisciplineConfigDistrictYearID, ConfigDistrictYearID = F, DistrictID = F, SchoolYearID = F, RestartIncidentNumberThisYear = F, AllowOnlyOneOffensePerIncident = F, DefaultDisciplineScreenDateAndTimes = F, DefaultOffenseValueFromPreviousPerson = F, DefaultActionValueFromPreviousPerson = F, UsePerceivedMotivation = F, UseIncidentBuildingAndRoom = F, ConfigDistrictYearIDClonedFrom = F, AllowUseOfWarning = F, AllowActionRecommendationsOnReferrals = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DisplayWarningsInFamilyAndStudentAccess = F, DaysToDelayDisplayOfIncidentsInFamilyAndStudentAccess = F, AllowActionTypeUpdate = F, DisplayStudentOffensesForAllEntities = F, DisplayInvolvedPersonsFromAllEntities = F, AllowDurationTypeUpdate = F, DefaultActionStatus = F, DefaultGuardianNotifiedOnActionDetailFromAction = F, IsReadOnlyHistoricalRecord = F, AllowInternalComments = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DisciplineConfigDistrictYearID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "ConfigDistrictYear", objectId = DisciplineConfigDistrictYearID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PerceivedMotivations
	#'
	#' This function returns a dataframe or json object of PerceivedMotivations
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('PerceivedMotivation') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of PerceivedMotivations
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPerceivedMotivations <- function(searchConditionsList = NULL, PerceivedMotivationID = F, DistrictID = F, SchoolYearID = F, Code = F, Description = F, CodeDescription = F, PerceivedMotivationIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "PerceivedMotivation", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get PerceivedMotivation
	#'
	#' This function returns a dataframe or json object of a PerceivedMotivation
	#' @param PerceivedMotivationID The ID of the PerceivedMotivation to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('PerceivedMotivation') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getPerceivedMotivation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPerceivedMotivation <- function(PerceivedMotivationID, DistrictID = F, SchoolYearID = F, Code = F, Description = F, CodeDescription = F, PerceivedMotivationIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PerceivedMotivationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "PerceivedMotivation", objectId = PerceivedMotivationID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DisciplineLocations
	#'
	#' This function returns a dataframe or json object of DisciplineLocations
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('DisciplineLocation') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of DisciplineLocations
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDisciplineLocations <- function(searchConditionsList = NULL, LocationID = F, DistrictID = F, Code = F, Description = F, EdFiIncidentLocationID = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, LocationMNID = F, StateIncidentLocationMNID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "Location", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get DisciplineLocation
	#'
	#' This function returns a dataframe or json object of a DisciplineLocation
	#' @param DisciplineLocationID The ID of the DisciplineLocation to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('DisciplineLocation') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getDisciplineLocation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDisciplineLocation <- function(DisciplineLocationID, LocationID = F, DistrictID = F, Code = F, Description = F, EdFiIncidentLocationID = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, LocationMNID = F, StateIncidentLocationMNID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DisciplineLocationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "Location", objectId = DisciplineLocationID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Offenses
	#'
	#' This function returns a dataframe or json object of Offenses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('Offense') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of Offenses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listOffenses <- function(searchConditionsList = NULL, OffenseID = F, DistrictID = F, SchoolYearID = F, Code = F, Description = F, RestrictActions = F, DefaultActionID = F, UseForReferral = F, AllowActionRecommendations = F, OffenseLevelIDDefault = F, CodeDescription = F, OffenseIDClonedFrom = F, FederalOffenseCategoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsDrugRelated = F, IsWeaponRelated = F, IsInjuryThreat = F, HarassmentBullying = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "Offense", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get Offense
	#'
	#' This function returns a dataframe or json object of an Offense
	#' @param OffenseID The ID of the Offense to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('Offense') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getOffense
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getOffense <- function(OffenseID, DistrictID = F, SchoolYearID = F, Code = F, Description = F, RestrictActions = F, DefaultActionID = F, UseForReferral = F, AllowActionRecommendations = F, OffenseLevelIDDefault = F, CodeDescription = F, OffenseIDClonedFrom = F, FederalOffenseCategoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsDrugRelated = F, IsWeaponRelated = F, IsInjuryThreat = F, HarassmentBullying = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "OffenseID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "Offense", objectId = OffenseID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List OffenseActions
	#'
	#' This function returns a dataframe or json object of OffenseActions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('OffenseAction') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of OffenseActions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listOffenseActions <- function(searchConditionsList = NULL, OffenseActionID = F, OffenseID = F, ActionID = F, OffenseActionIDClonedFrom = F, IsReferralAction = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "OffenseAction", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get OffenseAction
	#'
	#' This function returns a dataframe or json object of an OffenseAction
	#' @param OffenseActionID The ID of the OffenseAction to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('OffenseAction') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getOffenseAction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getOffenseAction <- function(OffenseActionID, OffenseID = F, ActionID = F, OffenseActionIDClonedFrom = F, IsReferralAction = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "OffenseActionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "OffenseAction", objectId = OffenseActionID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List OffenseLevels
	#'
	#' This function returns a dataframe or json object of OffenseLevels
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('OffenseLevel') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of OffenseLevels
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listOffenseLevels <- function(searchConditionsList = NULL, OffenseLevelID = F, DistrictID = F, SchoolYearID = F, Code = F, Description = F, CodeDescription = F, OffenseLevelIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "OffenseLevel", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get OffenseLevel
	#'
	#' This function returns a dataframe or json object of an OffenseLevel
	#' @param OffenseLevelID The ID of the OffenseLevel to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('OffenseLevel') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getOffenseLevel
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getOffenseLevel <- function(OffenseLevelID, DistrictID = F, SchoolYearID = F, Code = F, Description = F, CodeDescription = F, OffenseLevelIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "OffenseLevelID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "OffenseLevel", objectId = OffenseLevelID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Drugs
	#'
	#' This function returns a dataframe or json object of Drugs
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('Drug') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of Drugs
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDrugs <- function(searchConditionsList = NULL, DrugMNID = F, StateDrugTypeMNID = F, DrugID = F, DistrictID = F, SchoolYearID = F, Code = F, Description = F, DrugIDClonedFrom = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "Drug", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get Drug
	#'
	#' This function returns a dataframe or json object of a Drug
	#' @param DrugID The ID of the Drug to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('Drug') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getDrug
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDrug <- function(DrugID, DrugMNID = F, StateDrugTypeMNID = F, DistrictID = F, SchoolYearID = F, Code = F, Description = F, DrugIDClonedFrom = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DrugID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "Drug", objectId = DrugID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List IncidentOffenseNameDrugs
	#'
	#' This function returns a dataframe or json object of IncidentOffenseNameDrugs
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('IncidentOffenseNameDrug') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of IncidentOffenseNameDrugs
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listIncidentOffenseNameDrugs <- function(searchConditionsList = NULL, IncidentOffenseNameDrugID = F, IncidentOffenseNameID = F, DrugID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "IncidentOffenseNameDrug", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get IncidentOffenseNameDrug
	#'
	#' This function returns a dataframe or json object of an IncidentOffenseNameDrug
	#' @param IncidentOffenseNameDrugID The ID of the IncidentOffenseNameDrug to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('IncidentOffenseNameDrug') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getIncidentOffenseNameDrug
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getIncidentOffenseNameDrug <- function(IncidentOffenseNameDrugID, IncidentOffenseNameID = F, DrugID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "IncidentOffenseNameDrugID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "IncidentOffenseNameDrug", objectId = IncidentOffenseNameDrugID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List IncidentOffenseNameWeapons
	#'
	#' This function returns a dataframe or json object of IncidentOffenseNameWeapons
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('IncidentOffenseNameWeapon') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of IncidentOffenseNameWeapons
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listIncidentOffenseNameWeapons <- function(searchConditionsList = NULL, IncidentOffenseNameWeaponID = F, IncidentOffenseNameID = F, WeaponID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IncidentOffenseNameWeaponMNID = F, MeetsFederalStatuteOfDangerousWeapon = F, MeetsStateStatuteOfDangerousWeapon = F, GunWasLoaded = F, GunWasInCase = F, GunFoundInTrunk = F, WeaponCount = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "IncidentOffenseNameWeapon", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get IncidentOffenseNameWeapon
	#'
	#' This function returns a dataframe or json object of an IncidentOffenseNameWeapon
	#' @param IncidentOffenseNameWeaponID The ID of the IncidentOffenseNameWeapon to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('IncidentOffenseNameWeapon') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getIncidentOffenseNameWeapon
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getIncidentOffenseNameWeapon <- function(IncidentOffenseNameWeaponID, IncidentOffenseNameID = F, WeaponID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IncidentOffenseNameWeaponMNID = F, MeetsFederalStatuteOfDangerousWeapon = F, MeetsStateStatuteOfDangerousWeapon = F, GunWasLoaded = F, GunWasInCase = F, GunFoundInTrunk = F, WeaponCount = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "IncidentOffenseNameWeaponID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "IncidentOffenseNameWeapon", objectId = IncidentOffenseNameWeaponID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Weapons
	#'
	#' This function returns a dataframe or json object of Weapons
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('Weapon') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of Weapons
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listWeapons <- function(searchConditionsList = NULL, WeaponMNID = F, StateWeaponTypeMNID = F, StatePelletGunTypeMNID = F, WeaponID = F, DistrictID = F, SchoolYearID = F, Code = F, Description = F, WeaponIDClonedFrom = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "Weapon", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get Weapon
	#'
	#' This function returns a dataframe or json object of a Weapon
	#' @param WeaponID The ID of the Weapon to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('Weapon') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getWeapon
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getWeapon <- function(WeaponID, WeaponMNID = F, StateWeaponTypeMNID = F, StatePelletGunTypeMNID = F, DistrictID = F, SchoolYearID = F, Code = F, Description = F, WeaponIDClonedFrom = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "WeaponID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "Weapon", objectId = WeaponID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ActionTypes
	#'
	#' This function returns a dataframe or json object of ActionTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('ActionType') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of ActionTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listActionTypes <- function(searchConditionsList = NULL, ActionTypeID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, EdFiDisciplineDescriptorID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "ActionType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get ActionType
	#'
	#' This function returns a dataframe or json object of an ActionType
	#' @param ActionTypeID The ID of the ActionType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('ActionType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getActionType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getActionType <- function(ActionTypeID, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, EdFiDisciplineDescriptorID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ActionTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "ActionType", objectId = ActionTypeID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DisciplineConfigSystems
	#'
	#' This function returns a dataframe or json object of DisciplineConfigSystems
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('DisciplineConfigSystem') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of DisciplineConfigSystems
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDisciplineConfigSystems <- function(searchConditionsList = NULL, ConfigSystemID = F, AllowIncidentSuppression = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ReportIDDisciplineLetter = F, AllowUpdateHistoricalData = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "ConfigSystem", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get DisciplineConfigSystem
	#'
	#' This function returns a dataframe or json object of a DisciplineConfigSystem
	#' @param DisciplineConfigSystemID The ID of the DisciplineConfigSystem to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('DisciplineConfigSystem') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getDisciplineConfigSystem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDisciplineConfigSystem <- function(DisciplineConfigSystemID, ConfigSystemID = F, AllowIncidentSuppression = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ReportIDDisciplineLetter = F, AllowUpdateHistoricalData = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DisciplineConfigSystemID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "ConfigSystem", objectId = DisciplineConfigSystemID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DisciplineLetterTemplates
	#'
	#' This function returns a dataframe or json object of DisciplineLetterTemplates
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('DisciplineLetterTemplate') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of DisciplineLetterTemplates
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDisciplineLetterTemplates <- function(searchConditionsList = NULL, DisciplineLetterTemplateID = F, Description = F, Header = F, Body = F, Footer = F, DistrictID = F, SchoolYearID = F, DisciplineLetterTemplateIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, MediaIDLogo = F, GuardianNameFormat = F, StudentNameFormat = F, AdvisorNameFormat = F, SuperintendentNameFormat = F, ColumnHeaderLabel1 = F, ColumnHeaderLabel2 = F, ColumnHeaderLabel3 = F, ColumnHeaderLabel4 = F, ColumnHeaderLabel5 = F, ColumnHeaderLabel6 = F, ColumnHeaderLabel7 = F, ColumnHeaderLabel8 = F, ColumnHeaderLabel9 = F, ColumnHeaderLabel10 = F, IsDefault = F, ForCurrentEntity = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "DisciplineLetterTemplate", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get DisciplineLetterTemplate
	#'
	#' This function returns a dataframe or json object of a DisciplineLetterTemplate
	#' @param DisciplineLetterTemplateID The ID of the DisciplineLetterTemplate to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('DisciplineLetterTemplate') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getDisciplineLetterTemplate
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDisciplineLetterTemplate <- function(DisciplineLetterTemplateID, Description = F, Header = F, Body = F, Footer = F, DistrictID = F, SchoolYearID = F, DisciplineLetterTemplateIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, MediaIDLogo = F, GuardianNameFormat = F, StudentNameFormat = F, AdvisorNameFormat = F, SuperintendentNameFormat = F, ColumnHeaderLabel1 = F, ColumnHeaderLabel2 = F, ColumnHeaderLabel3 = F, ColumnHeaderLabel4 = F, ColumnHeaderLabel5 = F, ColumnHeaderLabel6 = F, ColumnHeaderLabel7 = F, ColumnHeaderLabel8 = F, ColumnHeaderLabel9 = F, ColumnHeaderLabel10 = F, IsDefault = F, ForCurrentEntity = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DisciplineLetterTemplateID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "DisciplineLetterTemplate", objectId = DisciplineLetterTemplateID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DisciplineLetterRunHistoryActions
	#'
	#' This function returns a dataframe or json object of DisciplineLetterRunHistoryActions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('DisciplineLetterRunHistoryAction') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of DisciplineLetterRunHistoryActions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDisciplineLetterRunHistoryActions <- function(searchConditionsList = NULL, DisciplineLetterRunHistoryActionID = F, ActionID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DisciplineLetterRunHistoryID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "DisciplineLetterRunHistoryAction", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get DisciplineLetterRunHistoryAction
	#'
	#' This function returns a dataframe or json object of a DisciplineLetterRunHistoryAction
	#' @param DisciplineLetterRunHistoryActionID The ID of the DisciplineLetterRunHistoryAction to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('DisciplineLetterRunHistoryAction') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getDisciplineLetterRunHistoryAction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDisciplineLetterRunHistoryAction <- function(DisciplineLetterRunHistoryActionID, ActionID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DisciplineLetterRunHistoryID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DisciplineLetterRunHistoryActionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "DisciplineLetterRunHistoryAction", objectId = DisciplineLetterRunHistoryActionID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DisciplineLetterTemplateEntities
	#'
	#' This function returns a dataframe or json object of DisciplineLetterTemplateEntities
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('DisciplineLetterTemplateEntity') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of DisciplineLetterTemplateEntities
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDisciplineLetterTemplateEntities <- function(searchConditionsList = NULL, DisciplineLetterTemplateEntityID = F, DisciplineLetterTemplateID = F, EntityID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "DisciplineLetterTemplateEntity", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get DisciplineLetterTemplateEntity
	#'
	#' This function returns a dataframe or json object of a DisciplineLetterTemplateEntity
	#' @param DisciplineLetterTemplateEntityID The ID of the DisciplineLetterTemplateEntity to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('DisciplineLetterTemplateEntity') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getDisciplineLetterTemplateEntity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDisciplineLetterTemplateEntity <- function(DisciplineLetterTemplateEntityID, DisciplineLetterTemplateID = F, EntityID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DisciplineLetterTemplateEntityID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "DisciplineLetterTemplateEntity", objectId = DisciplineLetterTemplateEntityID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DisciplineLetterRunHistoryOffenses
	#'
	#' This function returns a dataframe or json object of DisciplineLetterRunHistoryOffenses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('DisciplineLetterRunHistoryOffense') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of DisciplineLetterRunHistoryOffenses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDisciplineLetterRunHistoryOffenses <- function(searchConditionsList = NULL, DisciplineLetterRunHistoryOffenseID = F, DisciplineLetterRunHistoryID = F, OffenseID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "DisciplineLetterRunHistoryOffense", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get DisciplineLetterRunHistoryOffense
	#'
	#' This function returns a dataframe or json object of a DisciplineLetterRunHistoryOffense
	#' @param DisciplineLetterRunHistoryOffenseID The ID of the DisciplineLetterRunHistoryOffense to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('DisciplineLetterRunHistoryOffense') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getDisciplineLetterRunHistoryOffense
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDisciplineLetterRunHistoryOffense <- function(DisciplineLetterRunHistoryOffenseID, DisciplineLetterRunHistoryID = F, OffenseID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DisciplineLetterRunHistoryOffenseID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "DisciplineLetterRunHistoryOffense", objectId = DisciplineLetterRunHistoryOffenseID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List IncidentOffenseNameReportRunHistories
	#'
	#' This function returns a dataframe or json object of IncidentOffenseNameReportRunHistories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('IncidentOffenseNameReportRunHistory') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of IncidentOffenseNameReportRunHistories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listIncidentOffenseNameReportRunHistories <- function(searchConditionsList = NULL, IncidentOffenseNameReportRunHistoryID = F, DisciplineLetterRunHistoryID = F, IncidentOffenseNameID = F, StudentID = F, IsActive = F, AttachmentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ColumnHeaderData1 = F, ColumnHeaderData2 = F, ColumnHeaderData3 = F, ColumnHeaderData4 = F, ColumnHeaderData5 = F, ColumnHeaderData6 = F, ColumnHeaderData7 = F, ColumnHeaderData8 = F, ColumnHeaderData9 = F, ColumnHeaderData10 = F, UnboundHeader = F, UnboundBody = F, UnboundFooter = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "IncidentOffenseNameReportRunHistory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get IncidentOffenseNameReportRunHistory
	#'
	#' This function returns a dataframe or json object of an IncidentOffenseNameReportRunHistory
	#' @param IncidentOffenseNameReportRunHistoryID The ID of the IncidentOffenseNameReportRunHistory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('IncidentOffenseNameReportRunHistory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getIncidentOffenseNameReportRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getIncidentOffenseNameReportRunHistory <- function(IncidentOffenseNameReportRunHistoryID, DisciplineLetterRunHistoryID = F, IncidentOffenseNameID = F, StudentID = F, IsActive = F, AttachmentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ColumnHeaderData1 = F, ColumnHeaderData2 = F, ColumnHeaderData3 = F, ColumnHeaderData4 = F, ColumnHeaderData5 = F, ColumnHeaderData6 = F, ColumnHeaderData7 = F, ColumnHeaderData8 = F, ColumnHeaderData9 = F, ColumnHeaderData10 = F, UnboundHeader = F, UnboundBody = F, UnboundFooter = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "IncidentOffenseNameReportRunHistoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "IncidentOffenseNameReportRunHistory", objectId = IncidentOffenseNameReportRunHistoryID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DisciplineLetterTemplateHeaderColumns
	#'
	#' This function returns a dataframe or json object of DisciplineLetterTemplateHeaderColumns
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('DisciplineLetterTemplateHeaderColumn') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of DisciplineLetterTemplateHeaderColumns
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDisciplineLetterTemplateHeaderColumns <- function(searchConditionsList = NULL, DisciplineLetterTemplateHeaderColumnID = F, DisciplineLetterTemplateHeaderRowID = F, SortNumber = F, FieldType = F, LabelOverride = F, FreeformText = F, DisciplineLetterTemplateHeaderColumnIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ColumnNumber = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "DisciplineLetterTemplateHeaderColumn", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get DisciplineLetterTemplateHeaderColumn
	#'
	#' This function returns a dataframe or json object of a DisciplineLetterTemplateHeaderColumn
	#' @param DisciplineLetterTemplateHeaderColumnID The ID of the DisciplineLetterTemplateHeaderColumn to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('DisciplineLetterTemplateHeaderColumn') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getDisciplineLetterTemplateHeaderColumn
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDisciplineLetterTemplateHeaderColumn <- function(DisciplineLetterTemplateHeaderColumnID, DisciplineLetterTemplateHeaderRowID = F, SortNumber = F, FieldType = F, LabelOverride = F, FreeformText = F, DisciplineLetterTemplateHeaderColumnIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ColumnNumber = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DisciplineLetterTemplateHeaderColumnID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "DisciplineLetterTemplateHeaderColumn", objectId = DisciplineLetterTemplateHeaderColumnID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DisciplineLetterRunHistories
	#'
	#' This function returns a dataframe or json object of DisciplineLetterRunHistories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('DisciplineLetterRunHistory') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of DisciplineLetterRunHistories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDisciplineLetterRunHistories <- function(searchConditionsList = NULL, DisciplineLetterRunHistoryID = F, DisciplineLetterTemplateID = F, EntityID = F, RunDescription = F, ParameterData = F, ParameterDescription = F, Date = F, FilterType = F, IncidentType = F, GracePeriod = F, IsActive = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartDate = F, EndDate = F, ReportRunInfoID = F, SchoolYearID = F, FiscalYearID = F, CachedFiscalYear = F, SectionID = F, EntityIDList = F, SchoolYearNumericYearOrCurrent = F, CachedSchoolYear = F, CachedEntity = F, IsReadOnlyHistoricalRecord = F, PostToFASA = F, AttachmentDisplayName = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "DisciplineLetterRunHistory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get DisciplineLetterRunHistory
	#'
	#' This function returns a dataframe or json object of a DisciplineLetterRunHistory
	#' @param DisciplineLetterRunHistoryID The ID of the DisciplineLetterRunHistory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('DisciplineLetterRunHistory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getDisciplineLetterRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDisciplineLetterRunHistory <- function(DisciplineLetterRunHistoryID, DisciplineLetterTemplateID = F, EntityID = F, RunDescription = F, ParameterData = F, ParameterDescription = F, Date = F, FilterType = F, IncidentType = F, GracePeriod = F, IsActive = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartDate = F, EndDate = F, ReportRunInfoID = F, SchoolYearID = F, FiscalYearID = F, CachedFiscalYear = F, SectionID = F, EntityIDList = F, SchoolYearNumericYearOrCurrent = F, CachedSchoolYear = F, CachedEntity = F, IsReadOnlyHistoricalRecord = F, PostToFASA = F, AttachmentDisplayName = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DisciplineLetterRunHistoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "DisciplineLetterRunHistory", objectId = DisciplineLetterRunHistoryID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DisciplineLetterTemplateHeaderRows
	#'
	#' This function returns a dataframe or json object of DisciplineLetterTemplateHeaderRows
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('DisciplineLetterTemplateHeaderRow') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of DisciplineLetterTemplateHeaderRows
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDisciplineLetterTemplateHeaderRows <- function(searchConditionsList = NULL, DisciplineLetterTemplateHeaderRowID = F, DisciplineLetterTemplateID = F, SortNumber = F, ColumnCount = F, DisciplineLetterTemplateHeaderRowIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "DisciplineLetterTemplateHeaderRow", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get DisciplineLetterTemplateHeaderRow
	#'
	#' This function returns a dataframe or json object of a DisciplineLetterTemplateHeaderRow
	#' @param DisciplineLetterTemplateHeaderRowID The ID of the DisciplineLetterTemplateHeaderRow to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('DisciplineLetterTemplateHeaderRow') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getDisciplineLetterTemplateHeaderRow
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDisciplineLetterTemplateHeaderRow <- function(DisciplineLetterTemplateHeaderRowID, DisciplineLetterTemplateID = F, SortNumber = F, ColumnCount = F, DisciplineLetterTemplateHeaderRowIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DisciplineLetterTemplateHeaderRowID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "DisciplineLetterTemplateHeaderRow", objectId = DisciplineLetterTemplateHeaderRowID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempIncidentOffenseNameReportRunHistoryRecords
	#'
	#' This function returns a dataframe or json object of TempIncidentOffenseNameReportRunHistoryRecords
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempIncidentOffenseNameReportRunHistoryRecord') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of TempIncidentOffenseNameReportRunHistoryRecords
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempIncidentOffenseNameReportRunHistoryRecords <- function(searchConditionsList = NULL, TempIncidentOffenseNameReportRunHistoryRecordID = F, StudentID = F, StudentName = F, IncidentOffenseNameID = F, DateTime = F, Offense = F, Incident = F, IncidentNumber = F, ColumnHeader1 = F, ColumnHeader2 = F, ColumnHeader3 = F, ColumnHeader4 = F, ColumnHeader5 = F, ColumnHeader6 = F, ColumnHeader7 = F, ColumnHeader8 = F, ColumnHeader9 = F, ColumnHeader10 = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, UnboundHeader = F, UnboundBody = F, UnboundFooter = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffenseNameReportRunHistoryRecord", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempIncidentOffenseNameReportRunHistoryRecord
	#'
	#' This function returns a dataframe or json object of a TempIncidentOffenseNameReportRunHistoryRecord
	#' @param TempIncidentOffenseNameReportRunHistoryRecordID The ID of the TempIncidentOffenseNameReportRunHistoryRecord to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempIncidentOffenseNameReportRunHistoryRecord') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getTempIncidentOffenseNameReportRunHistoryRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempIncidentOffenseNameReportRunHistoryRecord <- function(TempIncidentOffenseNameReportRunHistoryRecordID, StudentID = F, StudentName = F, IncidentOffenseNameID = F, DateTime = F, Offense = F, Incident = F, IncidentNumber = F, ColumnHeader1 = F, ColumnHeader2 = F, ColumnHeader3 = F, ColumnHeader4 = F, ColumnHeader5 = F, ColumnHeader6 = F, ColumnHeader7 = F, ColumnHeader8 = F, ColumnHeader9 = F, ColumnHeader10 = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, UnboundHeader = F, UnboundBody = F, UnboundFooter = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempIncidentOffenseNameReportRunHistoryRecordID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameReportRunHistoryRecord", objectId = TempIncidentOffenseNameReportRunHistoryRecordID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List WhiteListFieldPaths
	#'
	#' This function returns a dataframe or json object of WhiteListFieldPaths
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('WhiteListFieldPath') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of WhiteListFieldPaths
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listWhiteListFieldPaths <- function(searchConditionsList = NULL, WhiteListFieldPathID = F, FriendlyName = F, Description = F, FieldPath = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, UsedForType = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "WhiteListFieldPath", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get WhiteListFieldPath
	#'
	#' This function returns a dataframe or json object of a WhiteListFieldPath
	#' @param WhiteListFieldPathID The ID of the WhiteListFieldPath to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('WhiteListFieldPath') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getWhiteListFieldPath
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getWhiteListFieldPath <- function(WhiteListFieldPathID, FriendlyName = F, Description = F, FieldPath = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, UsedForType = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "WhiteListFieldPathID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "WhiteListFieldPath", objectId = WhiteListFieldPathID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DisciplineConfigEntityYears
	#'
	#' This function returns a dataframe or json object of DisciplineConfigEntityYears
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('DisciplineConfigEntityYear') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of DisciplineConfigEntityYears
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDisciplineConfigEntityYears <- function(searchConditionsList = NULL, ConfigEntityYearID = F, EntityID = F, SchoolYearID = F, ConfigEntityYearIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "ConfigEntityYear", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get DisciplineConfigEntityYear
	#'
	#' This function returns a dataframe or json object of a DisciplineConfigEntityYear
	#' @param DisciplineConfigEntityYearID The ID of the DisciplineConfigEntityYear to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('DisciplineConfigEntityYear') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getDisciplineConfigEntityYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDisciplineConfigEntityYear <- function(DisciplineConfigEntityYearID, ConfigEntityYearID = F, EntityID = F, SchoolYearID = F, ConfigEntityYearIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DisciplineConfigEntityYearID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "ConfigEntityYear", objectId = DisciplineConfigEntityYearID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempIncidentOffenseNames
	#'
	#' This function returns a dataframe or json object of TempIncidentOffenseNames
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempIncidentOffenseName') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of TempIncidentOffenseNames
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempIncidentOffenseNames <- function(searchConditionsList = NULL, TempIncidentOffenseNameID = F, TempIncidentInvolvedPersonID = F, TempIncidentOffenseID = F, OffenseID = F, ExistingIncidentOffenseNameID = F, InvolvementType = F, PerceivedMotivationID = F, OffenseLevelID = F, FullName = F, StudentNumber = F, OffenseCodeDescription = F, IsPrimaryOffense = F, PerceivedMotivationCodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, InternalComment = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffenseName", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempIncidentOffenseName
	#'
	#' This function returns a dataframe or json object of a TempIncidentOffenseName
	#' @param TempIncidentOffenseNameID The ID of the TempIncidentOffenseName to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempIncidentOffenseName') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getTempIncidentOffenseName
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempIncidentOffenseName <- function(TempIncidentOffenseNameID, TempIncidentInvolvedPersonID = F, TempIncidentOffenseID = F, OffenseID = F, ExistingIncidentOffenseNameID = F, InvolvementType = F, PerceivedMotivationID = F, OffenseLevelID = F, FullName = F, StudentNumber = F, OffenseCodeDescription = F, IsPrimaryOffense = F, PerceivedMotivationCodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, InternalComment = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempIncidentOffenseNameID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "TempIncidentOffenseName", objectId = TempIncidentOffenseNameID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempIncidentOffenses
	#'
	#' This function returns a dataframe or json object of TempIncidentOffenses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempIncidentOffense') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of TempIncidentOffenses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempIncidentOffenses <- function(searchConditionsList = NULL, TempIncidentOffenseID = F, ExistingIncidentID = F, OffenseID = F, IsPrimaryOffense = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffense", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempIncidentOffense
	#'
	#' This function returns a dataframe or json object of a TempIncidentOffense
	#' @param TempIncidentOffenseID The ID of the TempIncidentOffense to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempIncidentOffense') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getTempIncidentOffense
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempIncidentOffense <- function(TempIncidentOffenseID, ExistingIncidentID = F, OffenseID = F, IsPrimaryOffense = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempIncidentOffenseID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "TempIncidentOffense", objectId = TempIncidentOffenseID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempIncidentOffenseNameParentalInvolvementPAS
	#'
	#' This function returns a dataframe or json object of TempIncidentOffenseNameParentalInvolvementPAS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempIncidentOffenseNameParentalInvolvementPA') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of TempIncidentOffenseNameParentalInvolvementPAS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempIncidentOffenseNameParentalInvolvementPAS <- function(searchConditionsList = NULL, TempIncidentOffenseNameParentalInvolvementPAID = F, IncidentOffenseNameParentalInvolvementPAID = F, TempIncidentOffenseNameID = F, StateParentalInvolvementPAID = F, Comment = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffenseNameParentalInvolvementPA", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempIncidentOffenseNameParentalInvolvementPA
	#'
	#' This function returns a dataframe or json object of a TempIncidentOffenseNameParentalInvolvementPA
	#' @param TempIncidentOffenseNameParentalInvolvementPAID The ID of the TempIncidentOffenseNameParentalInvolvementPA to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempIncidentOffenseNameParentalInvolvementPA') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getTempIncidentOffenseNameParentalInvolvementPA
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempIncidentOffenseNameParentalInvolvementPA <- function(TempIncidentOffenseNameParentalInvolvementPAID, IncidentOffenseNameParentalInvolvementPAID = F, TempIncidentOffenseNameID = F, StateParentalInvolvementPAID = F, Comment = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempIncidentOffenseNameParentalInvolvementPAID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameParentalInvolvementPA", objectId = TempIncidentOffenseNameParentalInvolvementPAID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempIncidentOffenseNameDrugs
	#'
	#' This function returns a dataframe or json object of TempIncidentOffenseNameDrugs
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempIncidentOffenseNameDrug') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of TempIncidentOffenseNameDrugs
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempIncidentOffenseNameDrugs <- function(searchConditionsList = NULL, TempIncidentOffenseNameDrugID = F, IncidentOffenseNameDrugID = F, TempIncidentOffenseNameID = F, DrugID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffenseNameDrug", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempIncidentOffenseNameDrug
	#'
	#' This function returns a dataframe or json object of a TempIncidentOffenseNameDrug
	#' @param TempIncidentOffenseNameDrugID The ID of the TempIncidentOffenseNameDrug to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempIncidentOffenseNameDrug') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getTempIncidentOffenseNameDrug
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempIncidentOffenseNameDrug <- function(TempIncidentOffenseNameDrugID, IncidentOffenseNameDrugID = F, TempIncidentOffenseNameID = F, DrugID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempIncidentOffenseNameDrugID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameDrug", objectId = TempIncidentOffenseNameDrugID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempIncidentOffenseNameWeaponMNS
	#'
	#' This function returns a dataframe or json object of TempIncidentOffenseNameWeaponMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempIncidentOffenseNameWeaponMN') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of TempIncidentOffenseNameWeaponMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempIncidentOffenseNameWeaponMNS <- function(searchConditionsList = NULL, MeetsFederalStatuteOfDangerousWeapon = F, MeetsStateStatuteOfDangerousWeapon = F, GunWasLoaded = F, GunWasInCase = F, GunFoundInTrunk = F, TempIncidentOffenseNameWeaponID = F, IncidentOffenseNameWeaponID = F, TempIncidentOffenseNameID = F, WeaponID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, WeaponCount = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffenseNameWeaponMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempIncidentOffenseNameWeaponMN
	#'
	#' This function returns a dataframe or json object of a TempIncidentOffenseNameWeaponMN
	#' @param TempIncidentOffenseNameWeaponMNID The ID of the TempIncidentOffenseNameWeaponMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempIncidentOffenseNameWeaponMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getTempIncidentOffenseNameWeaponMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempIncidentOffenseNameWeaponMN <- function(TempIncidentOffenseNameWeaponMNID, MeetsFederalStatuteOfDangerousWeapon = F, MeetsStateStatuteOfDangerousWeapon = F, GunWasLoaded = F, GunWasInCase = F, GunFoundInTrunk = F, TempIncidentOffenseNameWeaponID = F, IncidentOffenseNameWeaponID = F, TempIncidentOffenseNameID = F, WeaponID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, WeaponCount = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempIncidentOffenseNameWeaponMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameWeaponMN", objectId = TempIncidentOffenseNameWeaponMNID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempIncidentOffenseNameWeapons
	#'
	#' This function returns a dataframe or json object of TempIncidentOffenseNameWeapons
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempIncidentOffenseNameWeapon') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of TempIncidentOffenseNameWeapons
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempIncidentOffenseNameWeapons <- function(searchConditionsList = NULL, TempIncidentOffenseNameWeaponID = F, IncidentOffenseNameWeaponID = F, TempIncidentOffenseNameID = F, WeaponID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, WeaponCount = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffenseNameWeapon", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempIncidentOffenseNameWeapon
	#'
	#' This function returns a dataframe or json object of a TempIncidentOffenseNameWeapon
	#' @param TempIncidentOffenseNameWeaponID The ID of the TempIncidentOffenseNameWeapon to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempIncidentOffenseNameWeapon') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getTempIncidentOffenseNameWeapon
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempIncidentOffenseNameWeapon <- function(TempIncidentOffenseNameWeaponID, IncidentOffenseNameWeaponID = F, TempIncidentOffenseNameID = F, WeaponID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, WeaponCount = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempIncidentOffenseNameWeaponID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameWeapon", objectId = TempIncidentOffenseNameWeaponID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempIncidentOffenseNameActionWIS
	#'
	#' This function returns a dataframe or json object of TempIncidentOffenseNameActionWIS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempIncidentOffenseNameActionWI') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of TempIncidentOffenseNameActionWIS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempIncidentOffenseNameActionWIS <- function(searchConditionsList = NULL, StateModifiedTermWIID = F, HasEarlyReinstatementCondition = F, BehaviorDetailedDescription = F, IAESRemovalType = F, CausedSeriousBodilyInjury = F, TempIncidentOffenseNameActionID = F, EntityID = F, FullName = F, StudentNumber = F, InvolvementType = F, OffenseCodeDescription = F, IsPrimaryOffense = F, ActionTypeID = F, ActionTypeCode = F, ActionID = F, ActionCodeDescription = F, Status = F, OrderedDate = F, DurationType = F, DurationToServe = F, StaffIDAuthorizedBy = F, StaffIDAuthorizedByName = F, StaffIDFollowUpOfficer = F, LocationID = F, BuildingID = F, RoomID = F, PerceivedMotivationCodeDescription = F, Comment = F, TempIncidentOffenseNameID = F, IsGuardianNotified = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartTime = F, InternalComment = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffenseNameActionWI", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempIncidentOffenseNameActionWI
	#'
	#' This function returns a dataframe or json object of a TempIncidentOffenseNameActionWI
	#' @param TempIncidentOffenseNameActionWIID The ID of the TempIncidentOffenseNameActionWI to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempIncidentOffenseNameActionWI') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getTempIncidentOffenseNameActionWI
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempIncidentOffenseNameActionWI <- function(TempIncidentOffenseNameActionWIID, StateModifiedTermWIID = F, HasEarlyReinstatementCondition = F, BehaviorDetailedDescription = F, IAESRemovalType = F, CausedSeriousBodilyInjury = F, TempIncidentOffenseNameActionID = F, EntityID = F, FullName = F, StudentNumber = F, InvolvementType = F, OffenseCodeDescription = F, IsPrimaryOffense = F, ActionTypeID = F, ActionTypeCode = F, ActionID = F, ActionCodeDescription = F, Status = F, OrderedDate = F, DurationType = F, DurationToServe = F, StaffIDAuthorizedBy = F, StaffIDAuthorizedByName = F, StaffIDFollowUpOfficer = F, LocationID = F, BuildingID = F, RoomID = F, PerceivedMotivationCodeDescription = F, Comment = F, TempIncidentOffenseNameID = F, IsGuardianNotified = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartTime = F, InternalComment = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempIncidentOffenseNameActionWIID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameActionWI", objectId = TempIncidentOffenseNameActionWIID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempIncidentOffenseNameActionWAS
	#'
	#' This function returns a dataframe or json object of TempIncidentOffenseNameActionWAS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempIncidentOffenseNameActionWA') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of TempIncidentOffenseNameActionWAS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempIncidentOffenseNameActionWAS <- function(searchConditionsList = NULL, PlacedInInterimAlternativeEducationalSetting = F, DateReadmissionPetitionSubmitted = F, DateReadmissionPetitionGranted = F, DateReengagementMeetingHeld = F, StateAcademicServiceWAID = F, StateBehaviorServiceWAID = F, StatePetitionExtensionExpulsionWAID = F, StateReengagementPlanWAID = F, StateAppealCodeWAID = F, TotalAmountOfExclusionaryTime = F, DurationOfExclusionaryActionDays = F, TempIncidentOffenseNameActionID = F, EntityID = F, FullName = F, StudentNumber = F, InvolvementType = F, OffenseCodeDescription = F, IsPrimaryOffense = F, ActionTypeID = F, ActionTypeCode = F, ActionID = F, ActionCodeDescription = F, Status = F, OrderedDate = F, DurationType = F, DurationToServe = F, StaffIDAuthorizedBy = F, StaffIDAuthorizedByName = F, StaffIDFollowUpOfficer = F, LocationID = F, BuildingID = F, RoomID = F, PerceivedMotivationCodeDescription = F, Comment = F, TempIncidentOffenseNameID = F, IsGuardianNotified = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartTime = F, InternalComment = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffenseNameActionWA", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempIncidentOffenseNameActionWA
	#'
	#' This function returns a dataframe or json object of a TempIncidentOffenseNameActionWA
	#' @param TempIncidentOffenseNameActionWAID The ID of the TempIncidentOffenseNameActionWA to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempIncidentOffenseNameActionWA') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getTempIncidentOffenseNameActionWA
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempIncidentOffenseNameActionWA <- function(TempIncidentOffenseNameActionWAID, PlacedInInterimAlternativeEducationalSetting = F, DateReadmissionPetitionSubmitted = F, DateReadmissionPetitionGranted = F, DateReengagementMeetingHeld = F, StateAcademicServiceWAID = F, StateBehaviorServiceWAID = F, StatePetitionExtensionExpulsionWAID = F, StateReengagementPlanWAID = F, StateAppealCodeWAID = F, TotalAmountOfExclusionaryTime = F, DurationOfExclusionaryActionDays = F, TempIncidentOffenseNameActionID = F, EntityID = F, FullName = F, StudentNumber = F, InvolvementType = F, OffenseCodeDescription = F, IsPrimaryOffense = F, ActionTypeID = F, ActionTypeCode = F, ActionID = F, ActionCodeDescription = F, Status = F, OrderedDate = F, DurationType = F, DurationToServe = F, StaffIDAuthorizedBy = F, StaffIDAuthorizedByName = F, StaffIDFollowUpOfficer = F, LocationID = F, BuildingID = F, RoomID = F, PerceivedMotivationCodeDescription = F, Comment = F, TempIncidentOffenseNameID = F, IsGuardianNotified = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartTime = F, InternalComment = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempIncidentOffenseNameActionWAID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameActionWA", objectId = TempIncidentOffenseNameActionWAID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempIncidentOffenseNameActionPAS
	#'
	#' This function returns a dataframe or json object of TempIncidentOffenseNameActionPAS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempIncidentOffenseNameActionPA') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of TempIncidentOffenseNameActionPAS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempIncidentOffenseNameActionPAS <- function(searchConditionsList = NULL, ReceivedEducationalServices = F, AssignedAlternativeEducation = F, TempIncidentOffenseNameActionID = F, EntityID = F, FullName = F, StudentNumber = F, InvolvementType = F, OffenseCodeDescription = F, IsPrimaryOffense = F, ActionTypeID = F, ActionTypeCode = F, ActionID = F, ActionCodeDescription = F, Status = F, OrderedDate = F, DurationType = F, DurationToServe = F, StaffIDAuthorizedBy = F, StaffIDAuthorizedByName = F, StaffIDFollowUpOfficer = F, LocationID = F, BuildingID = F, RoomID = F, PerceivedMotivationCodeDescription = F, Comment = F, TempIncidentOffenseNameID = F, IsGuardianNotified = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartTime = F, InternalComment = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffenseNameActionPA", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempIncidentOffenseNameActionPA
	#'
	#' This function returns a dataframe or json object of a TempIncidentOffenseNameActionPA
	#' @param TempIncidentOffenseNameActionPAID The ID of the TempIncidentOffenseNameActionPA to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempIncidentOffenseNameActionPA') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getTempIncidentOffenseNameActionPA
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempIncidentOffenseNameActionPA <- function(TempIncidentOffenseNameActionPAID, ReceivedEducationalServices = F, AssignedAlternativeEducation = F, TempIncidentOffenseNameActionID = F, EntityID = F, FullName = F, StudentNumber = F, InvolvementType = F, OffenseCodeDescription = F, IsPrimaryOffense = F, ActionTypeID = F, ActionTypeCode = F, ActionID = F, ActionCodeDescription = F, Status = F, OrderedDate = F, DurationType = F, DurationToServe = F, StaffIDAuthorizedBy = F, StaffIDAuthorizedByName = F, StaffIDFollowUpOfficer = F, LocationID = F, BuildingID = F, RoomID = F, PerceivedMotivationCodeDescription = F, Comment = F, TempIncidentOffenseNameID = F, IsGuardianNotified = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartTime = F, InternalComment = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempIncidentOffenseNameActionPAID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameActionPA", objectId = TempIncidentOffenseNameActionPAID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempIncidentOffenseNameActionMNS
	#'
	#' This function returns a dataframe or json object of TempIncidentOffenseNameActionMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempIncidentOffenseNameActionMN') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of TempIncidentOffenseNameActionMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempIncidentOffenseNameActionMNS <- function(searchConditionsList = NULL, StateDIRSAESTypeMNID = F, DateExpulsionExclusionEnds = F, ReturnBeforeYearEnd = F, ExclusionThroughYearEnd = F, ExpulsionModified = F, ExpulsionThroughYearEnd = F, DIRSActionExplanation = F, TempIncidentOffenseNameActionID = F, EntityID = F, FullName = F, StudentNumber = F, InvolvementType = F, OffenseCodeDescription = F, IsPrimaryOffense = F, ActionTypeID = F, ActionTypeCode = F, ActionID = F, ActionCodeDescription = F, Status = F, OrderedDate = F, DurationType = F, DurationToServe = F, StaffIDAuthorizedBy = F, StaffIDAuthorizedByName = F, StaffIDFollowUpOfficer = F, LocationID = F, BuildingID = F, RoomID = F, PerceivedMotivationCodeDescription = F, Comment = F, TempIncidentOffenseNameID = F, IsGuardianNotified = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartTime = F, InternalComment = F, NoServiceProvidedExplanation = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffenseNameActionMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempIncidentOffenseNameActionMN
	#'
	#' This function returns a dataframe or json object of a TempIncidentOffenseNameActionMN
	#' @param TempIncidentOffenseNameActionMNID The ID of the TempIncidentOffenseNameActionMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempIncidentOffenseNameActionMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getTempIncidentOffenseNameActionMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempIncidentOffenseNameActionMN <- function(TempIncidentOffenseNameActionMNID, StateDIRSAESTypeMNID = F, DateExpulsionExclusionEnds = F, ReturnBeforeYearEnd = F, ExclusionThroughYearEnd = F, ExpulsionModified = F, ExpulsionThroughYearEnd = F, DIRSActionExplanation = F, TempIncidentOffenseNameActionID = F, EntityID = F, FullName = F, StudentNumber = F, InvolvementType = F, OffenseCodeDescription = F, IsPrimaryOffense = F, ActionTypeID = F, ActionTypeCode = F, ActionID = F, ActionCodeDescription = F, Status = F, OrderedDate = F, DurationType = F, DurationToServe = F, StaffIDAuthorizedBy = F, StaffIDAuthorizedByName = F, StaffIDFollowUpOfficer = F, LocationID = F, BuildingID = F, RoomID = F, PerceivedMotivationCodeDescription = F, Comment = F, TempIncidentOffenseNameID = F, IsGuardianNotified = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartTime = F, InternalComment = F, NoServiceProvidedExplanation = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempIncidentOffenseNameActionMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameActionMN", objectId = TempIncidentOffenseNameActionMNID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempIncidentOffenseNameActionINS
	#'
	#' This function returns a dataframe or json object of TempIncidentOffenseNameActionINS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempIncidentOffenseNameActionIN') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of TempIncidentOffenseNameActionINS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempIncidentOffenseNameActionINS <- function(searchConditionsList = NULL, StateSuspensionReasonINID = F, StateEducationalServiceProvidedINID = F, TempIncidentOffenseNameActionID = F, EntityID = F, FullName = F, StudentNumber = F, InvolvementType = F, OffenseCodeDescription = F, IsPrimaryOffense = F, ActionTypeID = F, ActionTypeCode = F, ActionID = F, ActionCodeDescription = F, Status = F, OrderedDate = F, DurationType = F, DurationToServe = F, StaffIDAuthorizedBy = F, StaffIDAuthorizedByName = F, StaffIDFollowUpOfficer = F, LocationID = F, BuildingID = F, RoomID = F, PerceivedMotivationCodeDescription = F, Comment = F, TempIncidentOffenseNameID = F, IsGuardianNotified = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartTime = F, IsPrimaryAction = F, InternalComment = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffenseNameActionIN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempIncidentOffenseNameActionIN
	#'
	#' This function returns a dataframe or json object of a TempIncidentOffenseNameActionIN
	#' @param TempIncidentOffenseNameActionINID The ID of the TempIncidentOffenseNameActionIN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempIncidentOffenseNameActionIN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getTempIncidentOffenseNameActionIN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempIncidentOffenseNameActionIN <- function(TempIncidentOffenseNameActionINID, StateSuspensionReasonINID = F, StateEducationalServiceProvidedINID = F, TempIncidentOffenseNameActionID = F, EntityID = F, FullName = F, StudentNumber = F, InvolvementType = F, OffenseCodeDescription = F, IsPrimaryOffense = F, ActionTypeID = F, ActionTypeCode = F, ActionID = F, ActionCodeDescription = F, Status = F, OrderedDate = F, DurationType = F, DurationToServe = F, StaffIDAuthorizedBy = F, StaffIDAuthorizedByName = F, StaffIDFollowUpOfficer = F, LocationID = F, BuildingID = F, RoomID = F, PerceivedMotivationCodeDescription = F, Comment = F, TempIncidentOffenseNameID = F, IsGuardianNotified = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartTime = F, IsPrimaryAction = F, InternalComment = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempIncidentOffenseNameActionINID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "TempIncidentOffenseNameActionIN", objectId = TempIncidentOffenseNameActionINID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempIncidentInvolvedPersonWAS
	#'
	#' This function returns a dataframe or json object of TempIncidentInvolvedPersonWAS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempIncidentInvolvedPersonWA') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of TempIncidentInvolvedPersonWAS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempIncidentInvolvedPersonWAS <- function(searchConditionsList = NULL, StateReportedOffense = F, TempIncidentInvolvedPersonID = F, IncidentOffenseNameKey = F, PrimaryOffenseID = F, SecondaryOffensesBackingData = F, ExcludePrimaryOffense = F, ExistingIncidentOffenseNamesToDelete = F, InvolvementType = F, IncidentOffenseNameType = F, FreeformName = F, StudentID = F, StaffID = F, NameID = F, StaffIDDisciplineOfficer = F, PerceivedMotivationID = F, FullName = F, StudentNumber = F, PerceivedMotivationCodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OSSCount = F, OSSPartialCount = F, ISSCount = F, ISSPartialCount = F, InternalComment = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentInvolvedPersonWA", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempIncidentInvolvedPersonWA
	#'
	#' This function returns a dataframe or json object of a TempIncidentInvolvedPersonWA
	#' @param TempIncidentInvolvedPersonWAID The ID of the TempIncidentInvolvedPersonWA to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempIncidentInvolvedPersonWA') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getTempIncidentInvolvedPersonWA
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempIncidentInvolvedPersonWA <- function(TempIncidentInvolvedPersonWAID, StateReportedOffense = F, TempIncidentInvolvedPersonID = F, IncidentOffenseNameKey = F, PrimaryOffenseID = F, SecondaryOffensesBackingData = F, ExcludePrimaryOffense = F, ExistingIncidentOffenseNamesToDelete = F, InvolvementType = F, IncidentOffenseNameType = F, FreeformName = F, StudentID = F, StaffID = F, NameID = F, StaffIDDisciplineOfficer = F, PerceivedMotivationID = F, FullName = F, StudentNumber = F, PerceivedMotivationCodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OSSCount = F, OSSPartialCount = F, ISSCount = F, ISSPartialCount = F, InternalComment = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempIncidentInvolvedPersonWAID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "TempIncidentInvolvedPersonWA", objectId = TempIncidentInvolvedPersonWAID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempIncidentInvolvedPersonTXES
	#'
	#' This function returns a dataframe or json object of TempIncidentInvolvedPersonTXES
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempIncidentInvolvedPersonTX') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of TempIncidentInvolvedPersonTXES
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempIncidentInvolvedPersonTXES <- function(searchConditionsList = NULL, CampusIDOfDisciplinaryResponsibility = F, TempIncidentInvolvedPersonID = F, IncidentOffenseNameKey = F, PrimaryOffenseID = F, SecondaryOffensesBackingData = F, ExcludePrimaryOffense = F, ExistingIncidentOffenseNamesToDelete = F, InvolvementType = F, IncidentOffenseNameType = F, FreeformName = F, StudentID = F, StaffID = F, NameID = F, StaffIDDisciplineOfficer = F, PerceivedMotivationID = F, FullName = F, StudentNumber = F, PerceivedMotivationCodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OSSCount = F, OSSPartialCount = F, ISSCount = F, ISSPartialCount = F, InternalComment = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentInvolvedPersonTX", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempIncidentInvolvedPersonTX
	#'
	#' This function returns a dataframe or json object of a TempIncidentInvolvedPersonTX
	#' @param TempIncidentInvolvedPersonTXID The ID of the TempIncidentInvolvedPersonTX to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempIncidentInvolvedPersonTX') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getTempIncidentInvolvedPersonTX
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempIncidentInvolvedPersonTX <- function(TempIncidentInvolvedPersonTXID, CampusIDOfDisciplinaryResponsibility = F, TempIncidentInvolvedPersonID = F, IncidentOffenseNameKey = F, PrimaryOffenseID = F, SecondaryOffensesBackingData = F, ExcludePrimaryOffense = F, ExistingIncidentOffenseNamesToDelete = F, InvolvementType = F, IncidentOffenseNameType = F, FreeformName = F, StudentID = F, StaffID = F, NameID = F, StaffIDDisciplineOfficer = F, PerceivedMotivationID = F, FullName = F, StudentNumber = F, PerceivedMotivationCodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OSSCount = F, OSSPartialCount = F, ISSCount = F, ISSPartialCount = F, InternalComment = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempIncidentInvolvedPersonTXID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "TempIncidentInvolvedPersonTX", objectId = TempIncidentInvolvedPersonTXID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempIncidentInvolvedPersonPAS
	#'
	#' This function returns a dataframe or json object of TempIncidentInvolvedPersonPAS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempIncidentInvolvedPersonPA') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of TempIncidentInvolvedPersonPAS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempIncidentInvolvedPersonPAS <- function(searchConditionsList = NULL, StateVictimTypePAID = F, StudentAssistanceProgramReferral = F, MedicalTreatmentRequired = F, IncidentVictimComment = F, StateInjurySeverityPAID = F, StateOffenderTypePAID = F, StateArrestedPAID = F, StateAdjudicationPAID = F, StateWeaponDetectedMethodPAID = F, WeaponDetectionComment = F, LocalLawEnforcementNotified = F, NameOfLocalLawEnforcementContacted = F, TempIncidentInvolvedPersonID = F, IncidentOffenseNameKey = F, PrimaryOffenseID = F, SecondaryOffensesBackingData = F, ExcludePrimaryOffense = F, ExistingIncidentOffenseNamesToDelete = F, InvolvementType = F, IncidentOffenseNameType = F, FreeformName = F, StudentID = F, StaffID = F, NameID = F, StaffIDDisciplineOfficer = F, PerceivedMotivationID = F, FullName = F, StudentNumber = F, PerceivedMotivationCodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsResidentialPlacementByNonEdAgency = F, LLEIncidentNumber = F, OSSCount = F, OSSPartialCount = F, ISSCount = F, ISSPartialCount = F, InternalComment = F, StateDistrictPAIDPerson = F, PASecureID = F, FirstName = F, LastName = F, RaceEthnicities = F, Genders = F, SendingOrCharterTypes = F, SendingDistrictOrCharterAUNID = F, AgeAtTimeOfIncident = F, StateGradeLevelPIMSPAID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentInvolvedPersonPA", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempIncidentInvolvedPersonPA
	#'
	#' This function returns a dataframe or json object of a TempIncidentInvolvedPersonPA
	#' @param TempIncidentInvolvedPersonPAID The ID of the TempIncidentInvolvedPersonPA to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempIncidentInvolvedPersonPA') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getTempIncidentInvolvedPersonPA
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempIncidentInvolvedPersonPA <- function(TempIncidentInvolvedPersonPAID, StateVictimTypePAID = F, StudentAssistanceProgramReferral = F, MedicalTreatmentRequired = F, IncidentVictimComment = F, StateInjurySeverityPAID = F, StateOffenderTypePAID = F, StateArrestedPAID = F, StateAdjudicationPAID = F, StateWeaponDetectedMethodPAID = F, WeaponDetectionComment = F, LocalLawEnforcementNotified = F, NameOfLocalLawEnforcementContacted = F, TempIncidentInvolvedPersonID = F, IncidentOffenseNameKey = F, PrimaryOffenseID = F, SecondaryOffensesBackingData = F, ExcludePrimaryOffense = F, ExistingIncidentOffenseNamesToDelete = F, InvolvementType = F, IncidentOffenseNameType = F, FreeformName = F, StudentID = F, StaffID = F, NameID = F, StaffIDDisciplineOfficer = F, PerceivedMotivationID = F, FullName = F, StudentNumber = F, PerceivedMotivationCodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsResidentialPlacementByNonEdAgency = F, LLEIncidentNumber = F, OSSCount = F, OSSPartialCount = F, ISSCount = F, ISSPartialCount = F, InternalComment = F, StateDistrictPAIDPerson = F, PASecureID = F, FirstName = F, LastName = F, RaceEthnicities = F, Genders = F, SendingOrCharterTypes = F, SendingDistrictOrCharterAUNID = F, AgeAtTimeOfIncident = F, StateGradeLevelPIMSPAID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempIncidentInvolvedPersonPAID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "TempIncidentInvolvedPersonPA", objectId = TempIncidentInvolvedPersonPAID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempIncidentInvolvedPersonMNS
	#'
	#' This function returns a dataframe or json object of TempIncidentInvolvedPersonMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempIncidentInvolvedPersonMN') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of TempIncidentInvolvedPersonMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempIncidentInvolvedPersonMNS <- function(searchConditionsList = NULL, StateVictimTypeMNID = F, StateVictimCostMNID = F, InjuryOccured = F, WasSeriousBodilyInjury = F, ReportedToLawEnforcement = F, OffenderArrestedByLawEnforcement = F, StateOffenderActivityMNID = F, IsPhysicalAssault = F, IsPhysicalAssaultState = F, TempIncidentInvolvedPersonID = F, IncidentOffenseNameKey = F, PrimaryOffenseID = F, SecondaryOffensesBackingData = F, ExcludePrimaryOffense = F, ExistingIncidentOffenseNamesToDelete = F, InvolvementType = F, IncidentOffenseNameType = F, FreeformName = F, StudentID = F, StaffID = F, NameID = F, StaffIDDisciplineOfficer = F, PerceivedMotivationID = F, FullName = F, StudentNumber = F, PerceivedMotivationCodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OSSCount = F, OSSPartialCount = F, ISSCount = F, ISSPartialCount = F, InternalComment = F, MultipleVictimCount = F, EstimatedVictimsEnrolled = F, EstimatedVictimsNotEnrolled = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentInvolvedPersonMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempIncidentInvolvedPersonMN
	#'
	#' This function returns a dataframe or json object of a TempIncidentInvolvedPersonMN
	#' @param TempIncidentInvolvedPersonMNID The ID of the TempIncidentInvolvedPersonMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempIncidentInvolvedPersonMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getTempIncidentInvolvedPersonMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempIncidentInvolvedPersonMN <- function(TempIncidentInvolvedPersonMNID, StateVictimTypeMNID = F, StateVictimCostMNID = F, InjuryOccured = F, WasSeriousBodilyInjury = F, ReportedToLawEnforcement = F, OffenderArrestedByLawEnforcement = F, StateOffenderActivityMNID = F, IsPhysicalAssault = F, IsPhysicalAssaultState = F, TempIncidentInvolvedPersonID = F, IncidentOffenseNameKey = F, PrimaryOffenseID = F, SecondaryOffensesBackingData = F, ExcludePrimaryOffense = F, ExistingIncidentOffenseNamesToDelete = F, InvolvementType = F, IncidentOffenseNameType = F, FreeformName = F, StudentID = F, StaffID = F, NameID = F, StaffIDDisciplineOfficer = F, PerceivedMotivationID = F, FullName = F, StudentNumber = F, PerceivedMotivationCodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OSSCount = F, OSSPartialCount = F, ISSCount = F, ISSPartialCount = F, InternalComment = F, MultipleVictimCount = F, EstimatedVictimsEnrolled = F, EstimatedVictimsNotEnrolled = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempIncidentInvolvedPersonMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "TempIncidentInvolvedPersonMN", objectId = TempIncidentInvolvedPersonMNID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempIncidentInvolvedPersonINS
	#'
	#' This function returns a dataframe or json object of TempIncidentInvolvedPersonINS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempIncidentInvolvedPersonIN') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of TempIncidentInvolvedPersonINS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempIncidentInvolvedPersonINS <- function(searchConditionsList = NULL, StateCriminalEventINID = F, StateArrestTypeINID = F, StateArrestReasonINID = F, IsChemicalRestraint = F, IsMechanicalRestraint = F, IsPhysicalRestraint = F, IsSeclusion = F, StaffIDResourceOfficer = F, TempIncidentInvolvedPersonID = F, IncidentOffenseNameKey = F, PrimaryOffenseID = F, SecondaryOffensesBackingData = F, ExcludePrimaryOffense = F, ExistingIncidentOffenseNamesToDelete = F, InvolvementType = F, IncidentOffenseNameType = F, FreeformName = F, StudentID = F, StaffID = F, NameID = F, StaffIDDisciplineOfficer = F, PerceivedMotivationID = F, FullName = F, StudentNumber = F, PerceivedMotivationCodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OSSCount = F, OSSPartialCount = F, ISSCount = F, ISSPartialCount = F, InternalComment = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentInvolvedPersonIN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempIncidentInvolvedPersonIN
	#'
	#' This function returns a dataframe or json object of a TempIncidentInvolvedPersonIN
	#' @param TempIncidentInvolvedPersonINID The ID of the TempIncidentInvolvedPersonIN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempIncidentInvolvedPersonIN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getTempIncidentInvolvedPersonIN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempIncidentInvolvedPersonIN <- function(TempIncidentInvolvedPersonINID, StateCriminalEventINID = F, StateArrestTypeINID = F, StateArrestReasonINID = F, IsChemicalRestraint = F, IsMechanicalRestraint = F, IsPhysicalRestraint = F, IsSeclusion = F, StaffIDResourceOfficer = F, TempIncidentInvolvedPersonID = F, IncidentOffenseNameKey = F, PrimaryOffenseID = F, SecondaryOffensesBackingData = F, ExcludePrimaryOffense = F, ExistingIncidentOffenseNamesToDelete = F, InvolvementType = F, IncidentOffenseNameType = F, FreeformName = F, StudentID = F, StaffID = F, NameID = F, StaffIDDisciplineOfficer = F, PerceivedMotivationID = F, FullName = F, StudentNumber = F, PerceivedMotivationCodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OSSCount = F, OSSPartialCount = F, ISSCount = F, ISSPartialCount = F, InternalComment = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempIncidentInvolvedPersonINID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "TempIncidentInvolvedPersonIN", objectId = TempIncidentInvolvedPersonINID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempIncidentInvolvedPeople
	#'
	#' This function returns a dataframe or json object of TempIncidentInvolvedPeople
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempIncidentInvolvedPerson') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A list of TempIncidentInvolvedPeople
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempIncidentInvolvedPeople <- function(searchConditionsList = NULL, TempIncidentInvolvedPersonID = F, IncidentOffenseNameKey = F, PrimaryOffenseID = F, SecondaryOffensesBackingData = F, ExcludePrimaryOffense = F, ExistingIncidentOffenseNamesToDelete = F, InvolvementType = F, IncidentOffenseNameType = F, FreeformName = F, StudentID = F, StaffID = F, NameID = F, StaffIDDisciplineOfficer = F, PerceivedMotivationID = F, FullName = F, StudentNumber = F, PerceivedMotivationCodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OSSCount = F, OSSPartialCount = F, ISSCount = F, ISSPartialCount = F, InternalComment = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentInvolvedPerson", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get TempIncidentInvolvedPerson
	#'
	#' This function returns a dataframe or json object of a TempIncidentInvolvedPerson
	#' @param TempIncidentInvolvedPersonID The ID of the TempIncidentInvolvedPerson to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('TempIncidentInvolvedPerson') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Discipline
	#' @return A dataframe or of getTempIncidentInvolvedPerson
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempIncidentInvolvedPerson <- function(TempIncidentInvolvedPersonID, IncidentOffenseNameKey = F, PrimaryOffenseID = F, SecondaryOffensesBackingData = F, ExcludePrimaryOffense = F, ExistingIncidentOffenseNamesToDelete = F, InvolvementType = F, IncidentOffenseNameType = F, FreeformName = F, StudentID = F, StaffID = F, NameID = F, StaffIDDisciplineOfficer = F, PerceivedMotivationID = F, FullName = F, StudentNumber = F, PerceivedMotivationCodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OSSCount = F, OSSPartialCount = F, ISSCount = F, ISSPartialCount = F, InternalComment = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempIncidentInvolvedPersonID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Discipline", objectName = "TempIncidentInvolvedPerson", objectId = TempIncidentInvolvedPersonID, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}
