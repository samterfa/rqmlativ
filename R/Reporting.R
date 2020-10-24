
	#' List ReportingUserSettings
	#'
	#' This function returns a dataframe or json object of ReportingUserSettings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReportingUserSettings. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReportingUserSettings.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReportingUserSetting') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of ReportingUserSettings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listReportingUserSettings <- function(searchConditionsList = NULL, UserSettingID = F, ShowSideBar = F, SideBarWidth = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DataMiningShowLeftFieldSelectionPanel = F, DataMiningLeftFieldSelectionPanelWidth = F, DataMiningMiddleFieldSelectionPanelWidth = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "UserSetting", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ReportingUserSetting
	#'
	#' This function returns a dataframe or json object of a ReportingUserSetting
	#' @param ReportingUserSettingID The ID of the ReportingUserSetting to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReportingUserSetting. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReportingUserSetting.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReportingUserSetting') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of ReportingUserSetting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getReportingUserSetting <- function(ReportingUserSettingID, UserSettingID = F, ShowSideBar = F, SideBarWidth = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DataMiningShowLeftFieldSelectionPanel = F, DataMiningLeftFieldSelectionPanelWidth = F, DataMiningMiddleFieldSelectionPanelWidth = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ReportingUserSettingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "UserSetting", objectId = ReportingUserSettingID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ReportingUserSetting
	#'
	#' This function deletes a ReportingUserSetting
	#' @param ReportingUserSettingID The ID of the ReportingUserSetting to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The ReportingUserSettingID of the deleted ReportingUserSetting.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteReportingUserSetting <- function(ReportingUserSettingID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "UserSetting", objectId = ReportingUserSettingID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ReportingUserSetting
	#'
	#' This function creates a ReportingUserSetting
	#' @param fieldNames The field values to give the created ReportingUserSetting. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created ReportingUserSetting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createReportingUserSetting <- function(ShowSideBar = NULL, SideBarWidth = NULL, DataMiningShowLeftFieldSelectionPanel = NULL, DataMiningLeftFieldSelectionPanelWidth = NULL, DataMiningMiddleFieldSelectionPanelWidth = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "UserSetting", body = list(DataObject = body), searchFields = append("UserSettingID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ReportingUserSetting
	#'
	#' This function modifies a ReportingUserSetting
	#' @param fieldNames The field values to give the modified ReportingUserSetting. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified ReportingUserSetting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyReportingUserSetting <- function(UserSettingID, ShowSideBar = NULL, SideBarWidth = NULL, DataMiningShowLeftFieldSelectionPanel = NULL, DataMiningLeftFieldSelectionPanelWidth = NULL, DataMiningMiddleFieldSelectionPanelWidth = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "UserSetting", objectId = UserSettingID, body = list(DataObject = body), searchFields = append("UserSettingID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ScheduledReports
	#'
	#' This function returns a dataframe or json object of ScheduledReports
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ScheduledReports. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ScheduledReports.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ScheduledReport') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of ScheduledReports
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listScheduledReports <- function(searchConditionsList = NULL, ScheduledReportID = F, ReportID = F, ReportName = F, SkywardReportSystemVersion = F, EntityID = F, EntityIDList = F, SchoolYearID = F, FiscalYearID = F, SectionID = F, SchoolYearNumericYearOrCurrent = F, CachedSchoolYear = F, CachedEntity = F, CachedFiscalYear = F, ScheduledTaskID = F, DaysToSaveReport = F, MessageMasterID = F, Name = F, NotifyByEmail = F, NotifyByMessageCenter = F, ExportFileName = F, AutomateFileName = F, OverwriteExistingFile = F, ReportType = F, ReportDefinition = F, PromptValues = F, RunCount = F, DefinitionUpdatedTime = F, ReportIsCurrent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ReportFileExtensionOverride = F, IsCrystalReport = F, PromptXML = F, MediaIDCrystalRPT = F, AutoUpdate = F, EncodingType = F, UserIDOwner = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "ScheduledReport", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ScheduledReport
	#'
	#' This function returns a dataframe or json object of a ScheduledReport
	#' @param ScheduledReportID The ID of the ScheduledReport to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ScheduledReport. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ScheduledReport.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ScheduledReport') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of ScheduledReport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getScheduledReport <- function(ScheduledReportID, ReportID = F, ReportName = F, SkywardReportSystemVersion = F, EntityID = F, EntityIDList = F, SchoolYearID = F, FiscalYearID = F, SectionID = F, SchoolYearNumericYearOrCurrent = F, CachedSchoolYear = F, CachedEntity = F, CachedFiscalYear = F, ScheduledTaskID = F, DaysToSaveReport = F, MessageMasterID = F, Name = F, NotifyByEmail = F, NotifyByMessageCenter = F, ExportFileName = F, AutomateFileName = F, OverwriteExistingFile = F, ReportType = F, ReportDefinition = F, PromptValues = F, RunCount = F, DefinitionUpdatedTime = F, ReportIsCurrent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ReportFileExtensionOverride = F, IsCrystalReport = F, PromptXML = F, MediaIDCrystalRPT = F, AutoUpdate = F, EncodingType = F, UserIDOwner = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ScheduledReportID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "ScheduledReport", objectId = ScheduledReportID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ScheduledReport
	#'
	#' This function deletes a ScheduledReport
	#' @param ScheduledReportID The ID of the ScheduledReport to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The ScheduledReportID of the deleted ScheduledReport.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteScheduledReport <- function(ScheduledReportID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "ScheduledReport", objectId = ScheduledReportID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ScheduledReport
	#'
	#' This function creates a ScheduledReport
	#' @param fieldNames The field values to give the created ScheduledReport. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created ScheduledReport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createScheduledReport <- function(ReportID = NULL, ReportName = NULL, SkywardReportSystemVersion = NULL, EntityID = NULL, SectionID = NULL, ScheduledTaskID = NULL, DaysToSaveReport = NULL, MessageMasterID = NULL, Name = NULL, NotifyByEmail = NULL, NotifyByMessageCenter = NULL, ExportFileName = NULL, AutomateFileName = NULL, OverwriteExistingFile = NULL, ReportType = NULL, DefinitionUpdatedTime = NULL, ReportFileExtensionOverride = NULL, MediaIDCrystalRPT = NULL, AutoUpdate = NULL, EncodingType = NULL, UserIDOwner = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "ScheduledReport", body = list(DataObject = body), searchFields = append("ScheduledReportID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ScheduledReport
	#'
	#' This function modifies a ScheduledReport
	#' @param fieldNames The field values to give the modified ScheduledReport. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified ScheduledReport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyScheduledReport <- function(ScheduledReportID, ReportID = NULL, ReportName = NULL, SkywardReportSystemVersion = NULL, EntityID = NULL, SectionID = NULL, ScheduledTaskID = NULL, DaysToSaveReport = NULL, MessageMasterID = NULL, Name = NULL, NotifyByEmail = NULL, NotifyByMessageCenter = NULL, ExportFileName = NULL, AutomateFileName = NULL, OverwriteExistingFile = NULL, ReportType = NULL, DefinitionUpdatedTime = NULL, ReportFileExtensionOverride = NULL, MediaIDCrystalRPT = NULL, AutoUpdate = NULL, EncodingType = NULL, UserIDOwner = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "ScheduledReport", objectId = ScheduledReportID, body = list(DataObject = body), searchFields = append("ScheduledReportID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ReportMenuModules
	#'
	#' This function returns a dataframe or json object of ReportMenuModules
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReportMenuModules. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReportMenuModules.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReportMenuModule') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of ReportMenuModules
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listReportMenuModules <- function(searchConditionsList = NULL, ReportID = F, MenuModuleID = F, IsPrimary = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, IsSkywardReportMenuModule = F, SkywardHash = F, EffectiveIsPrimary = F, IsPrimaryOverride = F, ReportMenuModuleID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "ReportMenuModule", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ReportMenuModule
	#'
	#' This function returns a dataframe or json object of a ReportMenuModule
	#' @param ReportMenuModuleID The ID of the ReportMenuModule to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReportMenuModule. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReportMenuModule.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReportMenuModule') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of ReportMenuModule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getReportMenuModule <- function(ReportMenuModuleID, ReportID = F, MenuModuleID = F, IsPrimary = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, IsSkywardReportMenuModule = F, SkywardHash = F, EffectiveIsPrimary = F, IsPrimaryOverride = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ReportMenuModuleID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "ReportMenuModule", objectId = ReportMenuModuleID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ReportMenuModule
	#'
	#' This function deletes a ReportMenuModule
	#' @param ReportMenuModuleID The ID of the ReportMenuModule to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The ReportMenuModuleID of the deleted ReportMenuModule.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteReportMenuModule <- function(ReportMenuModuleID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "ReportMenuModule", objectId = ReportMenuModuleID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ReportMenuModule
	#'
	#' This function creates a ReportMenuModule
	#' @param fieldNames The field values to give the created ReportMenuModule. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created ReportMenuModule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createReportMenuModule <- function(ReportID = NULL, MenuModuleID = NULL, IsPrimary = NULL, IsPrimaryOverride = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "ReportMenuModule", body = list(DataObject = body), searchFields = append("ReportMenuModuleID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ReportMenuModule
	#'
	#' This function modifies a ReportMenuModule
	#' @param fieldNames The field values to give the modified ReportMenuModule. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified ReportMenuModule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyReportMenuModule <- function(ReportMenuModuleID, ReportID = NULL, MenuModuleID = NULL, IsPrimary = NULL, IsPrimaryOverride = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "ReportMenuModule", objectId = ReportMenuModuleID, body = list(DataObject = body), searchFields = append("ReportMenuModuleID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ReportStyles
	#'
	#' This function returns a dataframe or json object of ReportStyles
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReportStyles. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReportStyles.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReportStyle') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of ReportStyles
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listReportStyles <- function(searchConditionsList = NULL, ReportStyleID = F, Name = F, SkywardID = F, IsSkywardReportStyle = F, IsNotSkywardReportStyle = F, ReportDefinition = F, IsPublic = F, CurrentUserCanDelete = F, CurrentUserCanMakePublic = F, CurrentUserCanMakeNotPublic = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "ReportStyle", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ReportStyle
	#'
	#' This function returns a dataframe or json object of a ReportStyle
	#' @param ReportStyleID The ID of the ReportStyle to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReportStyle. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReportStyle.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReportStyle') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of ReportStyle
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getReportStyle <- function(ReportStyleID, Name = F, SkywardID = F, IsSkywardReportStyle = F, IsNotSkywardReportStyle = F, ReportDefinition = F, IsPublic = F, CurrentUserCanDelete = F, CurrentUserCanMakePublic = F, CurrentUserCanMakeNotPublic = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ReportStyleID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "ReportStyle", objectId = ReportStyleID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ReportStyle
	#'
	#' This function deletes a ReportStyle
	#' @param ReportStyleID The ID of the ReportStyle to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The ReportStyleID of the deleted ReportStyle.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteReportStyle <- function(ReportStyleID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "ReportStyle", objectId = ReportStyleID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ReportStyle
	#'
	#' This function creates a ReportStyle
	#' @param fieldNames The field values to give the created ReportStyle. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created ReportStyle
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createReportStyle <- function(Name = NULL, IsPublic = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "ReportStyle", body = list(DataObject = body), searchFields = append("ReportStyleID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ReportStyle
	#'
	#' This function modifies a ReportStyle
	#' @param fieldNames The field values to give the modified ReportStyle. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified ReportStyle
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyReportStyle <- function(ReportStyleID, Name = NULL, IsPublic = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "ReportStyle", objectId = ReportStyleID, body = list(DataObject = body), searchFields = append("ReportStyleID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ReportQueueSQLS
	#'
	#' This function returns a dataframe or json object of ReportQueueSQLS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReportQueueSQLS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReportQueueSQLS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReportQueueSQL') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of ReportQueueSQLS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listReportQueueSQLS <- function(searchConditionsList = NULL, ReportQueueSQLID = F, ReportQueueID = F, ExecutedQuery = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "ReportQueueSQL", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ReportQueueSQL
	#'
	#' This function returns a dataframe or json object of a ReportQueueSQL
	#' @param ReportQueueSQLID The ID of the ReportQueueSQL to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReportQueueSQL. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReportQueueSQL.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReportQueueSQL') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of ReportQueueSQL
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getReportQueueSQL <- function(ReportQueueSQLID, ReportQueueID = F, ExecutedQuery = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ReportQueueSQLID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "ReportQueueSQL", objectId = ReportQueueSQLID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ReportQueueSQL
	#'
	#' This function deletes a ReportQueueSQL
	#' @param ReportQueueSQLID The ID of the ReportQueueSQL to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The ReportQueueSQLID of the deleted ReportQueueSQL.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteReportQueueSQL <- function(ReportQueueSQLID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "ReportQueueSQL", objectId = ReportQueueSQLID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ReportQueueSQL
	#'
	#' This function creates a ReportQueueSQL
	#' @param fieldNames The field values to give the created ReportQueueSQL. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created ReportQueueSQL
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createReportQueueSQL <- function(ReportQueueID = NULL, ExecutedQuery = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "ReportQueueSQL", body = list(DataObject = body), searchFields = append("ReportQueueSQLID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ReportQueueSQL
	#'
	#' This function modifies a ReportQueueSQL
	#' @param fieldNames The field values to give the modified ReportQueueSQL. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified ReportQueueSQL
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyReportQueueSQL <- function(ReportQueueSQLID, ReportQueueID = NULL, ExecutedQuery = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "ReportQueueSQL", objectId = ReportQueueSQLID, body = list(DataObject = body), searchFields = append("ReportQueueSQLID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List UserReportPrompts
	#'
	#' This function returns a dataframe or json object of UserReportPrompts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given UserReportPrompts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the UserReportPrompts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('UserReportPrompt') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of UserReportPrompts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listUserReportPrompts <- function(searchConditionsList = NULL, UserReportPromptID = F, ReportID = F, SkywardPromptValues = F, CrystalPromptValues = F, EntityID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PromptTemplateID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "UserReportPrompt", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an UserReportPrompt
	#'
	#' This function returns a dataframe or json object of an UserReportPrompt
	#' @param UserReportPromptID The ID of the UserReportPrompt to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given UserReportPrompt. Defaults to FALSE for all return fields which, for convenience, returns all fields for the UserReportPrompt.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('UserReportPrompt') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of UserReportPrompt
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getUserReportPrompt <- function(UserReportPromptID, ReportID = F, SkywardPromptValues = F, CrystalPromptValues = F, EntityID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PromptTemplateID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "UserReportPromptID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "UserReportPrompt", objectId = UserReportPromptID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an UserReportPrompt
	#'
	#' This function deletes an UserReportPrompt
	#' @param UserReportPromptID The ID of the UserReportPrompt to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The UserReportPromptID of the deleted UserReportPrompt.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteUserReportPrompt <- function(UserReportPromptID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "UserReportPrompt", objectId = UserReportPromptID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an UserReportPrompt
	#'
	#' This function creates an UserReportPrompt
	#' @param fieldNames The field values to give the created UserReportPrompt. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created UserReportPrompt
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createUserReportPrompt <- function(ReportID = NULL, EntityID = NULL, PromptTemplateID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "UserReportPrompt", body = list(DataObject = body), searchFields = append("UserReportPromptID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an UserReportPrompt
	#'
	#' This function modifies an UserReportPrompt
	#' @param fieldNames The field values to give the modified UserReportPrompt. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified UserReportPrompt
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyUserReportPrompt <- function(UserReportPromptID, ReportID = NULL, EntityID = NULL, PromptTemplateID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "UserReportPrompt", objectId = UserReportPromptID, body = list(DataObject = body), searchFields = append("UserReportPromptID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Pages
	#'
	#' This function returns a dataframe or json object of Pages
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Pages. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Pages.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Page') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of Pages
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPages <- function(searchConditionsList = NULL, PageID = F, ReportQueueID = F, PageNumber = F, Tables = F, TotalHeightInPoints = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "Page", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Page
	#'
	#' This function returns a dataframe or json object of a Page
	#' @param PageID The ID of the Page to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Page. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Page.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Page') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of Page
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPage <- function(PageID, ReportQueueID = F, PageNumber = F, Tables = F, TotalHeightInPoints = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PageID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "Page", objectId = PageID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Page
	#'
	#' This function deletes a Page
	#' @param PageID The ID of the Page to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The PageID of the deleted Page.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePage <- function(PageID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "Page", objectId = PageID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Page
	#'
	#' This function creates a Page
	#' @param fieldNames The field values to give the created Page. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created Page
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPage <- function(ReportQueueID = NULL, PageNumber = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "Page", body = list(DataObject = body), searchFields = append("PageID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Page
	#'
	#' This function modifies a Page
	#' @param fieldNames The field values to give the modified Page. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified Page
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPage <- function(PageID, ReportQueueID = NULL, PageNumber = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "Page", objectId = PageID, body = list(DataObject = body), searchFields = append("PageID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Reports
	#'
	#' This function returns a dataframe or json object of Reports
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Reports. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Reports.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Report') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of Reports
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listReports <- function(searchConditionsList = NULL, ReportID = F, SkywardID = F, OriginReportSkywardID = F, SkywardReportSystemVersion = F, CrystalParameterData = F, ReportType = F, MediaIDCrystalRPT = F, IsSkywardReport = F, EffectiveDescription = F, Name = F, ModuleIDBase = F, ObjectIDBase = F, UserIDOwner = F, Modules = F, PublishedTime = F, HasUnpublishedChanges = F, Published = F, NotPublished = F, CanBeScheduled = F, CanBeAddedToScreens = F, CurrentUserCanRead = F, CurrentUserCanUpdate = F, CurrentUserCanClone = F, StateID = F, ContainsStateSpecificFields = F, CurrentUserCanSetStateID = F, RunCount = F, LastRunTime = F, AllowOthersToClone = F, SaveUntil = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CanRevert = F, Portal = F, CurrentUserIsEffectiveOwner = F, ModulePath = F, ObjectName = F, EffectiveDisplayInMainMenuAndListScreens = F, EffectiveFileExtensionOverride = F, StateSpecificFields = F, StateSpecificFieldsDisplay = F, IsCrystalReport = F, SkywardHash = F, UpdatesMadeToMasterReport = F, FileExtensionOverride = F, FileExtensionNewOverride = F, OverrideFileExtensionOverride = F, Description = F, DescriptionOverride = F, OverrideDescription = F, DisplayInMainMenuAndListScreens = F, DisplayInMainMenuAndListScreensOverride = F, OverrideDisplayInMainMenuAndListScreens = F, CurrentUserCanDelete = F, CurrentUserCanUpdateOverrideFields = F, IsSkywardMaintained = F, KeepDataSources = F, EncodingType = F, BurstActionIDPrintAction = F, IsSummaryReport = F, PublishedCalculatedInCSharp = F, DefinitionWasModified = F, HasNoUnpublishedChanges = F, HasErrors = F, PromptForFiscalYear = F, PublishedXML = F, WorkingXML = F, IsLicensed = F, IsWSIPCMaintained = F, IsDeactivatedReport = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "Report", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Report
	#'
	#' This function returns a dataframe or json object of a Report
	#' @param ReportID The ID of the Report to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Report. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Report.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Report') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of Report
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getReport <- function(ReportID, SkywardID = F, OriginReportSkywardID = F, SkywardReportSystemVersion = F, CrystalParameterData = F, ReportType = F, MediaIDCrystalRPT = F, IsSkywardReport = F, EffectiveDescription = F, Name = F, ModuleIDBase = F, ObjectIDBase = F, UserIDOwner = F, Modules = F, PublishedTime = F, HasUnpublishedChanges = F, Published = F, NotPublished = F, CanBeScheduled = F, CanBeAddedToScreens = F, CurrentUserCanRead = F, CurrentUserCanUpdate = F, CurrentUserCanClone = F, StateID = F, ContainsStateSpecificFields = F, CurrentUserCanSetStateID = F, RunCount = F, LastRunTime = F, AllowOthersToClone = F, SaveUntil = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CanRevert = F, Portal = F, CurrentUserIsEffectiveOwner = F, ModulePath = F, ObjectName = F, EffectiveDisplayInMainMenuAndListScreens = F, EffectiveFileExtensionOverride = F, StateSpecificFields = F, StateSpecificFieldsDisplay = F, IsCrystalReport = F, SkywardHash = F, UpdatesMadeToMasterReport = F, FileExtensionOverride = F, FileExtensionNewOverride = F, OverrideFileExtensionOverride = F, Description = F, DescriptionOverride = F, OverrideDescription = F, DisplayInMainMenuAndListScreens = F, DisplayInMainMenuAndListScreensOverride = F, OverrideDisplayInMainMenuAndListScreens = F, CurrentUserCanDelete = F, CurrentUserCanUpdateOverrideFields = F, IsSkywardMaintained = F, KeepDataSources = F, EncodingType = F, BurstActionIDPrintAction = F, IsSummaryReport = F, PublishedCalculatedInCSharp = F, DefinitionWasModified = F, HasNoUnpublishedChanges = F, HasErrors = F, PromptForFiscalYear = F, PublishedXML = F, WorkingXML = F, IsLicensed = F, IsWSIPCMaintained = F, IsDeactivatedReport = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ReportID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "Report", objectId = ReportID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Report
	#'
	#' This function deletes a Report
	#' @param ReportID The ID of the Report to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The ReportID of the deleted Report.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteReport <- function(ReportID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "Report", objectId = ReportID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Report
	#'
	#' This function creates a Report
	#' @param fieldNames The field values to give the created Report. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created Report
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createReport <- function(SkywardReportSystemVersion = NULL, CrystalParameterData = NULL, ReportType = NULL, MediaIDCrystalRPT = NULL, Name = NULL, ModuleIDBase = NULL, ObjectIDBase = NULL, UserIDOwner = NULL, PublishedTime = NULL, StateID = NULL, AllowOthersToClone = NULL, SaveUntil = NULL, Portal = NULL, FileExtensionOverride = NULL, FileExtensionNewOverride = NULL, Description = NULL, DescriptionOverride = NULL, DisplayInMainMenuAndListScreens = NULL, DisplayInMainMenuAndListScreensOverride = NULL, OverrideDisplayInMainMenuAndListScreens = NULL, IsSkywardMaintained = NULL, KeepDataSources = NULL, EncodingType = NULL, BurstActionIDPrintAction = NULL, PublishedXML = NULL, WorkingXML = NULL, IsWSIPCMaintained = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "Report", body = list(DataObject = body), searchFields = append("ReportID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Report
	#'
	#' This function modifies a Report
	#' @param fieldNames The field values to give the modified Report. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified Report
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyReport <- function(ReportID, SkywardReportSystemVersion = NULL, CrystalParameterData = NULL, ReportType = NULL, MediaIDCrystalRPT = NULL, Name = NULL, ModuleIDBase = NULL, ObjectIDBase = NULL, UserIDOwner = NULL, PublishedTime = NULL, StateID = NULL, AllowOthersToClone = NULL, SaveUntil = NULL, Portal = NULL, FileExtensionOverride = NULL, FileExtensionNewOverride = NULL, Description = NULL, DescriptionOverride = NULL, DisplayInMainMenuAndListScreens = NULL, DisplayInMainMenuAndListScreensOverride = NULL, OverrideDisplayInMainMenuAndListScreens = NULL, IsSkywardMaintained = NULL, KeepDataSources = NULL, EncodingType = NULL, BurstActionIDPrintAction = NULL, PublishedXML = NULL, WorkingXML = NULL, IsWSIPCMaintained = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "Report", objectId = ReportID, body = list(DataObject = body), searchFields = append("ReportID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ReportQueues
	#'
	#' This function returns a dataframe or json object of ReportQueues
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReportQueues. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReportQueues.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReportQueue') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of ReportQueues
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listReportQueues <- function(searchConditionsList = NULL, ReportQueueID = F, ReportID = F, ReportName = F, ReportType = F, SkywardReportSystemVersion = F, IsFromPublishedVersion = F, PageCountWhenCompleted = F, PageCount = F, LogID = F, SaveUntil = F, HasContent = F, HasContentViewable = F, HasContentDownloadable = F, StatusCode = F, Status = F, QueueCode = F, Queue = F, CanCancel = F, EntityID = F, EntityIDList = F, FiscalYearID = F, SchoolYearID = F, SectionID = F, SchoolYearNumericYearOrCurrent = F, SchoolYearIDSelectedOrCurrent = F, FiscalYearIDSelectedOrCurrent = F, Application = F, Hostname = F, ThreadName = F, ProcessID = F, ReferrerPath = F, MediaID = F, CrystalParameterData = F, ScheduledReportID = F, QueryStartTime = F, RenderingStartTime = F, EndTime = F, QueuedDuration = F, QueryDuration = F, RenderingDuration = F, CachedEntity = F, CachedSchoolYear = F, CachedFiscalYear = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CurrentUserCanView = F, ReportFileExtensionOverride = F, IsCrystalReport = F, LastActivity = F, StatusAction = F, PromptTemplateID = F, FTPResultID = F, DataSource = F, MediaIDPrint = F, MediaIDDownload = F, MediaIDCsv = F, EncodingType = F, Encoding = F, BurstObjectIDs = F, HasPrintAction = F, HideFiscalYear = F, HideSchoolYear = F, ReportQueueIDSummaryReportSource = F, ReportQueueIDSummaryReport = F, IsViewableReport = F, ResultStatus = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "ReportQueue", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ReportQueue
	#'
	#' This function returns a dataframe or json object of a ReportQueue
	#' @param ReportQueueID The ID of the ReportQueue to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReportQueue. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReportQueue.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReportQueue') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of ReportQueue
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getReportQueue <- function(ReportQueueID, ReportID = F, ReportName = F, ReportType = F, SkywardReportSystemVersion = F, IsFromPublishedVersion = F, PageCountWhenCompleted = F, PageCount = F, LogID = F, SaveUntil = F, HasContent = F, HasContentViewable = F, HasContentDownloadable = F, StatusCode = F, Status = F, QueueCode = F, Queue = F, CanCancel = F, EntityID = F, EntityIDList = F, FiscalYearID = F, SchoolYearID = F, SectionID = F, SchoolYearNumericYearOrCurrent = F, SchoolYearIDSelectedOrCurrent = F, FiscalYearIDSelectedOrCurrent = F, Application = F, Hostname = F, ThreadName = F, ProcessID = F, ReferrerPath = F, MediaID = F, CrystalParameterData = F, ScheduledReportID = F, QueryStartTime = F, RenderingStartTime = F, EndTime = F, QueuedDuration = F, QueryDuration = F, RenderingDuration = F, CachedEntity = F, CachedSchoolYear = F, CachedFiscalYear = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CurrentUserCanView = F, ReportFileExtensionOverride = F, IsCrystalReport = F, LastActivity = F, StatusAction = F, PromptTemplateID = F, FTPResultID = F, DataSource = F, MediaIDPrint = F, MediaIDDownload = F, MediaIDCsv = F, EncodingType = F, Encoding = F, BurstObjectIDs = F, HasPrintAction = F, HideFiscalYear = F, HideSchoolYear = F, ReportQueueIDSummaryReportSource = F, ReportQueueIDSummaryReport = F, IsViewableReport = F, ResultStatus = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ReportQueueID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "ReportQueue", objectId = ReportQueueID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ReportQueue
	#'
	#' This function deletes a ReportQueue
	#' @param ReportQueueID The ID of the ReportQueue to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The ReportQueueID of the deleted ReportQueue.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteReportQueue <- function(ReportQueueID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "ReportQueue", objectId = ReportQueueID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ReportQueue
	#'
	#' This function creates a ReportQueue
	#' @param fieldNames The field values to give the created ReportQueue. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created ReportQueue
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createReportQueue <- function(ReportID = NULL, ReportName = NULL, ReportType = NULL, SkywardReportSystemVersion = NULL, IsFromPublishedVersion = NULL, LogID = NULL, SaveUntil = NULL, Status = NULL, Queue = NULL, EntityID = NULL, FiscalYearID = NULL, SchoolYearID = NULL, SectionID = NULL, SchoolYearNumericYearOrCurrent = NULL, SchoolYearIDSelectedOrCurrent = NULL, FiscalYearIDSelectedOrCurrent = NULL, Application = NULL, Hostname = NULL, ThreadName = NULL, ProcessID = NULL, ReferrerPath = NULL, MediaID = NULL, CrystalParameterData = NULL, ScheduledReportID = NULL, QueryStartTime = NULL, RenderingStartTime = NULL, EndTime = NULL, ReportFileExtensionOverride = NULL, LastActivity = NULL, PromptTemplateID = NULL, FTPResultID = NULL, DataSource = NULL, MediaIDPrint = NULL, MediaIDDownload = NULL, MediaIDCsv = NULL, EncodingType = NULL, ReportQueueIDSummaryReportSource = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "ReportQueue", body = list(DataObject = body), searchFields = append("ReportQueueID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ReportQueue
	#'
	#' This function modifies a ReportQueue
	#' @param fieldNames The field values to give the modified ReportQueue. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified ReportQueue
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyReportQueue <- function(ReportQueueID, ReportID = NULL, ReportName = NULL, ReportType = NULL, SkywardReportSystemVersion = NULL, IsFromPublishedVersion = NULL, LogID = NULL, SaveUntil = NULL, Status = NULL, Queue = NULL, EntityID = NULL, FiscalYearID = NULL, SchoolYearID = NULL, SectionID = NULL, SchoolYearNumericYearOrCurrent = NULL, SchoolYearIDSelectedOrCurrent = NULL, FiscalYearIDSelectedOrCurrent = NULL, Application = NULL, Hostname = NULL, ThreadName = NULL, ProcessID = NULL, ReferrerPath = NULL, MediaID = NULL, CrystalParameterData = NULL, ScheduledReportID = NULL, QueryStartTime = NULL, RenderingStartTime = NULL, EndTime = NULL, ReportFileExtensionOverride = NULL, LastActivity = NULL, PromptTemplateID = NULL, FTPResultID = NULL, DataSource = NULL, MediaIDPrint = NULL, MediaIDDownload = NULL, MediaIDCsv = NULL, EncodingType = NULL, ReportQueueIDSummaryReportSource = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "ReportQueue", objectId = ReportQueueID, body = list(DataObject = body), searchFields = append("ReportQueueID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ReportRoles
	#'
	#' This function returns a dataframe or json object of ReportRoles
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReportRoles. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReportRoles.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReportRole') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of ReportRoles
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listReportRoles <- function(searchConditionsList = NULL, ReportRoleID = F, ReportID = F, RoleID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "ReportRole", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ReportRole
	#'
	#' This function returns a dataframe or json object of a ReportRole
	#' @param ReportRoleID The ID of the ReportRole to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReportRole. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReportRole.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReportRole') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of ReportRole
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getReportRole <- function(ReportRoleID, ReportID = F, RoleID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ReportRoleID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "ReportRole", objectId = ReportRoleID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ReportRole
	#'
	#' This function deletes a ReportRole
	#' @param ReportRoleID The ID of the ReportRole to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The ReportRoleID of the deleted ReportRole.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteReportRole <- function(ReportRoleID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "ReportRole", objectId = ReportRoleID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ReportRole
	#'
	#' This function creates a ReportRole
	#' @param fieldNames The field values to give the created ReportRole. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created ReportRole
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createReportRole <- function(ReportID = NULL, RoleID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "ReportRole", body = list(DataObject = body), searchFields = append("ReportRoleID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ReportRole
	#'
	#' This function modifies a ReportRole
	#' @param fieldNames The field values to give the modified ReportRole. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified ReportRole
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyReportRole <- function(ReportRoleID, ReportID = NULL, RoleID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "ReportRole", objectId = ReportRoleID, body = list(DataObject = body), searchFields = append("ReportRoleID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DataMiningReportFields
	#'
	#' This function returns a dataframe or json object of DataMiningReportFields
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DataMiningReportFields. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DataMiningReportFields.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DataMiningReportField') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of DataMiningReportFields
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDataMiningReportFields <- function(searchConditionsList = NULL, DataMiningReportFieldID = F, DataMiningReportID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Label = F, Width = F, SubtopicFieldID = F, DisplayOrder = F, CurrentUserCanRead = F, ShowTotal = F, StartPosition = F, EndPosition = F, WidthInPixels = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "DataMiningReportField", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DataMiningReportField
	#'
	#' This function returns a dataframe or json object of a DataMiningReportField
	#' @param DataMiningReportFieldID The ID of the DataMiningReportField to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DataMiningReportField. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DataMiningReportField.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DataMiningReportField') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of DataMiningReportField
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDataMiningReportField <- function(DataMiningReportFieldID, DataMiningReportID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Label = F, Width = F, SubtopicFieldID = F, DisplayOrder = F, CurrentUserCanRead = F, ShowTotal = F, StartPosition = F, EndPosition = F, WidthInPixels = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DataMiningReportFieldID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "DataMiningReportField", objectId = DataMiningReportFieldID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DataMiningReportField
	#'
	#' This function deletes a DataMiningReportField
	#' @param DataMiningReportFieldID The ID of the DataMiningReportField to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The DataMiningReportFieldID of the deleted DataMiningReportField.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDataMiningReportField <- function(DataMiningReportFieldID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "DataMiningReportField", objectId = DataMiningReportFieldID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DataMiningReportField
	#'
	#' This function creates a DataMiningReportField
	#' @param fieldNames The field values to give the created DataMiningReportField. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created DataMiningReportField
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDataMiningReportField <- function(DataMiningReportID = NULL, Label = NULL, Width = NULL, SubtopicFieldID = NULL, DisplayOrder = NULL, ShowTotal = NULL, StartPosition = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "DataMiningReportField", body = list(DataObject = body), searchFields = append("DataMiningReportFieldID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DataMiningReportField
	#'
	#' This function modifies a DataMiningReportField
	#' @param fieldNames The field values to give the modified DataMiningReportField. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified DataMiningReportField
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDataMiningReportField <- function(DataMiningReportFieldID, DataMiningReportID = NULL, Label = NULL, Width = NULL, SubtopicFieldID = NULL, DisplayOrder = NULL, ShowTotal = NULL, StartPosition = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "DataMiningReportField", objectId = DataMiningReportFieldID, body = list(DataObject = body), searchFields = append("DataMiningReportFieldID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Subtopics
	#'
	#' This function returns a dataframe or json object of Subtopics
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Subtopics. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Subtopics.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Subtopic') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of Subtopics
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSubtopics <- function(searchConditionsList = NULL, SubtopicID = F, SkywardID = F, SkywardHash = F, ObjectID = F, RelationshipPath = F, Name = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CurrentUserHasAccess = F, IsOneToMany = F, OneToManyRelationshipPath = F, FieldAreaPath = F, DisplayOrder = F, DefaultSortDirection = F, CustomizationID = F, UniqueID = F, IsSkywardLoaded = F, SubtopicIDParent = F, SubjectID = F, DefaultSortGuidFieldPath = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "Subtopic", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Subtopic
	#'
	#' This function returns a dataframe or json object of a Subtopic
	#' @param SubtopicID The ID of the Subtopic to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Subtopic. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Subtopic.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Subtopic') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of Subtopic
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSubtopic <- function(SubtopicID, SkywardID = F, SkywardHash = F, ObjectID = F, RelationshipPath = F, Name = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CurrentUserHasAccess = F, IsOneToMany = F, OneToManyRelationshipPath = F, FieldAreaPath = F, DisplayOrder = F, DefaultSortDirection = F, CustomizationID = F, UniqueID = F, IsSkywardLoaded = F, SubtopicIDParent = F, SubjectID = F, DefaultSortGuidFieldPath = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SubtopicID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "Subtopic", objectId = SubtopicID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Subtopic
	#'
	#' This function deletes a Subtopic
	#' @param SubtopicID The ID of the Subtopic to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The SubtopicID of the deleted Subtopic.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSubtopic <- function(SubtopicID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "Subtopic", objectId = SubtopicID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Subtopic
	#'
	#' This function creates a Subtopic
	#' @param fieldNames The field values to give the created Subtopic. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created Subtopic
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSubtopic <- function(ObjectID = NULL, RelationshipPath = NULL, Name = NULL, IsOneToMany = NULL, DisplayOrder = NULL, SubtopicIDParent = NULL, SubjectID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "Subtopic", body = list(DataObject = body), searchFields = append("SubtopicID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Subtopic
	#'
	#' This function modifies a Subtopic
	#' @param fieldNames The field values to give the modified Subtopic. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified Subtopic
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySubtopic <- function(SubtopicID, ObjectID = NULL, RelationshipPath = NULL, Name = NULL, IsOneToMany = NULL, DisplayOrder = NULL, SubtopicIDParent = NULL, SubjectID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "Subtopic", objectId = SubtopicID, body = list(DataObject = body), searchFields = append("SubtopicID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DataMiningReports
	#'
	#' This function returns a dataframe or json object of DataMiningReports
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DataMiningReports. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DataMiningReports.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DataMiningReport') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of DataMiningReports
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDataMiningReports <- function(searchConditionsList = NULL, DataMiningReportID = F, Name = F, ReportID = F, CurrentUserCanRun = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ReportSchemaObject = F, HasFieldsThatCanBeTotaled = F, CurrentUserCanRead = F, TextQualifier = F, Delimiter = F, SubjectID = F, IsFixedWidth = F, CurrentUserCanEdit = F, IncludeSectionHeaders = F, DisplayOnSingleLine = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "DataMiningReport", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DataMiningReport
	#'
	#' This function returns a dataframe or json object of a DataMiningReport
	#' @param DataMiningReportID The ID of the DataMiningReport to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DataMiningReport. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DataMiningReport.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DataMiningReport') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of DataMiningReport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDataMiningReport <- function(DataMiningReportID, Name = F, ReportID = F, CurrentUserCanRun = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ReportSchemaObject = F, HasFieldsThatCanBeTotaled = F, CurrentUserCanRead = F, TextQualifier = F, Delimiter = F, SubjectID = F, IsFixedWidth = F, CurrentUserCanEdit = F, IncludeSectionHeaders = F, DisplayOnSingleLine = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DataMiningReportID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "DataMiningReport", objectId = DataMiningReportID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DataMiningReport
	#'
	#' This function deletes a DataMiningReport
	#' @param DataMiningReportID The ID of the DataMiningReport to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The DataMiningReportID of the deleted DataMiningReport.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDataMiningReport <- function(DataMiningReportID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "DataMiningReport", objectId = DataMiningReportID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DataMiningReport
	#'
	#' This function creates a DataMiningReport
	#' @param fieldNames The field values to give the created DataMiningReport. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created DataMiningReport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDataMiningReport <- function(ReportID = NULL, TextQualifier = NULL, Delimiter = NULL, SubjectID = NULL, IncludeSectionHeaders = NULL, DisplayOnSingleLine = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "DataMiningReport", body = list(DataObject = body), searchFields = append("DataMiningReportID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DataMiningReport
	#'
	#' This function modifies a DataMiningReport
	#' @param fieldNames The field values to give the modified DataMiningReport. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified DataMiningReport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDataMiningReport <- function(DataMiningReportID, ReportID = NULL, TextQualifier = NULL, Delimiter = NULL, SubjectID = NULL, IncludeSectionHeaders = NULL, DisplayOnSingleLine = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "DataMiningReport", objectId = DataMiningReportID, body = list(DataObject = body), searchFields = append("DataMiningReportID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ReportRunInfos
	#'
	#' This function returns a dataframe or json object of ReportRunInfos
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReportRunInfos. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReportRunInfos.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReportRunInfo') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of ReportRunInfos
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listReportRunInfos <- function(searchConditionsList = NULL, ReportRunInfoID = F, ReportID = F, ObjectID = F, PromptDataSources = F, SourceSchemaObject = F, SourceTypeName = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SecurityLocationReportSetSkywardID = F, ObjectSkywardID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "ReportRunInfo", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ReportRunInfo
	#'
	#' This function returns a dataframe or json object of a ReportRunInfo
	#' @param ReportRunInfoID The ID of the ReportRunInfo to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReportRunInfo. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReportRunInfo.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReportRunInfo') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of ReportRunInfo
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getReportRunInfo <- function(ReportRunInfoID, ReportID = F, ObjectID = F, PromptDataSources = F, SourceSchemaObject = F, SourceTypeName = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SecurityLocationReportSetSkywardID = F, ObjectSkywardID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ReportRunInfoID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "ReportRunInfo", objectId = ReportRunInfoID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ReportRunInfo
	#'
	#' This function deletes a ReportRunInfo
	#' @param ReportRunInfoID The ID of the ReportRunInfo to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The ReportRunInfoID of the deleted ReportRunInfo.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteReportRunInfo <- function(ReportRunInfoID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "ReportRunInfo", objectId = ReportRunInfoID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ReportRunInfo
	#'
	#' This function creates a ReportRunInfo
	#' @param fieldNames The field values to give the created ReportRunInfo. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created ReportRunInfo
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createReportRunInfo <- function(ReportID = NULL, ObjectID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "ReportRunInfo", body = list(DataObject = body), searchFields = append("ReportRunInfoID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ReportRunInfo
	#'
	#' This function modifies a ReportRunInfo
	#' @param fieldNames The field values to give the modified ReportRunInfo. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified ReportRunInfo
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyReportRunInfo <- function(ReportRunInfoID, ReportID = NULL, ObjectID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "ReportRunInfo", objectId = ReportRunInfoID, body = list(DataObject = body), searchFields = append("ReportRunInfoID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SecurityLocationReportSets
	#'
	#' This function returns a dataframe or json object of SecurityLocationReportSets
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SecurityLocationReportSets. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SecurityLocationReportSets.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SecurityLocationReportSet') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of SecurityLocationReportSets
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSecurityLocationReportSets <- function(searchConditionsList = NULL, SecurityLocationReportSetID = F, SecurityLocationID = F, DataObjectID = F, IsSkywardLoaded = F, Name = F, PrimaryKeySource = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, FullLocation = F, Module = F, AcceptsDataObject = F, Object = F, Screen = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "SecurityLocationReportSet", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SecurityLocationReportSet
	#'
	#' This function returns a dataframe or json object of a SecurityLocationReportSet
	#' @param SecurityLocationReportSetID The ID of the SecurityLocationReportSet to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SecurityLocationReportSet. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SecurityLocationReportSet.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SecurityLocationReportSet') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of SecurityLocationReportSet
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSecurityLocationReportSet <- function(SecurityLocationReportSetID, SecurityLocationID = F, DataObjectID = F, IsSkywardLoaded = F, Name = F, PrimaryKeySource = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, FullLocation = F, Module = F, AcceptsDataObject = F, Object = F, Screen = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SecurityLocationReportSetID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "SecurityLocationReportSet", objectId = SecurityLocationReportSetID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SecurityLocationReportSet
	#'
	#' This function deletes a SecurityLocationReportSet
	#' @param SecurityLocationReportSetID The ID of the SecurityLocationReportSet to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The SecurityLocationReportSetID of the deleted SecurityLocationReportSet.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSecurityLocationReportSet <- function(SecurityLocationReportSetID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "SecurityLocationReportSet", objectId = SecurityLocationReportSetID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SecurityLocationReportSet
	#'
	#' This function creates a SecurityLocationReportSet
	#' @param fieldNames The field values to give the created SecurityLocationReportSet. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created SecurityLocationReportSet
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSecurityLocationReportSet <- function(SecurityLocationID = NULL, DataObjectID = NULL, Name = NULL, PrimaryKeySource = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "SecurityLocationReportSet", body = list(DataObject = body), searchFields = append("SecurityLocationReportSetID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SecurityLocationReportSet
	#'
	#' This function modifies a SecurityLocationReportSet
	#' @param fieldNames The field values to give the modified SecurityLocationReportSet. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified SecurityLocationReportSet
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySecurityLocationReportSet <- function(SecurityLocationReportSetID, SecurityLocationID = NULL, DataObjectID = NULL, Name = NULL, PrimaryKeySource = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "SecurityLocationReportSet", objectId = SecurityLocationReportSetID, body = list(DataObject = body), searchFields = append("SecurityLocationReportSetID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SecurityLocationReportSetReportRunInfos
	#'
	#' This function returns a dataframe or json object of SecurityLocationReportSetReportRunInfos
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SecurityLocationReportSetReportRunInfos. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SecurityLocationReportSetReportRunInfos.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SecurityLocationReportSetReportRunInfo') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of SecurityLocationReportSetReportRunInfos
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSecurityLocationReportSetReportRunInfos <- function(searchConditionsList = NULL, SecurityLocationReportSetReportRunInfoID = F, SecurityLocationReportSetID = F, ReportRunInfoID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsSkywardLoaded = F, InUse = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "SecurityLocationReportSetReportRunInfo", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SecurityLocationReportSetReportRunInfo
	#'
	#' This function returns a dataframe or json object of a SecurityLocationReportSetReportRunInfo
	#' @param SecurityLocationReportSetReportRunInfoID The ID of the SecurityLocationReportSetReportRunInfo to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SecurityLocationReportSetReportRunInfo. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SecurityLocationReportSetReportRunInfo.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SecurityLocationReportSetReportRunInfo') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of SecurityLocationReportSetReportRunInfo
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSecurityLocationReportSetReportRunInfo <- function(SecurityLocationReportSetReportRunInfoID, SecurityLocationReportSetID = F, ReportRunInfoID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsSkywardLoaded = F, InUse = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SecurityLocationReportSetReportRunInfoID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "SecurityLocationReportSetReportRunInfo", objectId = SecurityLocationReportSetReportRunInfoID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SecurityLocationReportSetReportRunInfo
	#'
	#' This function deletes a SecurityLocationReportSetReportRunInfo
	#' @param SecurityLocationReportSetReportRunInfoID The ID of the SecurityLocationReportSetReportRunInfo to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The SecurityLocationReportSetReportRunInfoID of the deleted SecurityLocationReportSetReportRunInfo.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSecurityLocationReportSetReportRunInfo <- function(SecurityLocationReportSetReportRunInfoID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "SecurityLocationReportSetReportRunInfo", objectId = SecurityLocationReportSetReportRunInfoID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SecurityLocationReportSetReportRunInfo
	#'
	#' This function creates a SecurityLocationReportSetReportRunInfo
	#' @param fieldNames The field values to give the created SecurityLocationReportSetReportRunInfo. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created SecurityLocationReportSetReportRunInfo
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSecurityLocationReportSetReportRunInfo <- function(SecurityLocationReportSetID = NULL, ReportRunInfoID = NULL, IsSkywardLoaded = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "SecurityLocationReportSetReportRunInfo", body = list(DataObject = body), searchFields = append("SecurityLocationReportSetReportRunInfoID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SecurityLocationReportSetReportRunInfo
	#'
	#' This function modifies a SecurityLocationReportSetReportRunInfo
	#' @param fieldNames The field values to give the modified SecurityLocationReportSetReportRunInfo. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified SecurityLocationReportSetReportRunInfo
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySecurityLocationReportSetReportRunInfo <- function(SecurityLocationReportSetReportRunInfoID, SecurityLocationReportSetID = NULL, ReportRunInfoID = NULL, IsSkywardLoaded = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "SecurityLocationReportSetReportRunInfo", objectId = SecurityLocationReportSetReportRunInfoID, body = list(DataObject = body), searchFields = append("SecurityLocationReportSetReportRunInfoID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DataMiningReportFieldFilters
	#'
	#' This function returns a dataframe or json object of DataMiningReportFieldFilters
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DataMiningReportFieldFilters. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DataMiningReportFieldFilters.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DataMiningReportFieldFilter') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of DataMiningReportFieldFilters
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDataMiningReportFieldFilters <- function(searchConditionsList = NULL, DataMiningReportFieldFilterID = F, FilterType = F, Low = F, High = F, ListBackingData = F, ListDisplayValue = F, CurrentUserCanRead = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SubtopicFieldID = F, DataMiningReportID = F, DistinctMultiSelectSchemaObject = F, DistinctMultiSelectModule = F, DistinctMultiSelectObject = F, DistinctMultiSelectFieldPath = F, FormatedDisplayValue = F, IsPrompt = F, PromptLabel = F, PromptIsRequired = F, ComparisonType = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "DataMiningReportFieldFilter", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DataMiningReportFieldFilter
	#'
	#' This function returns a dataframe or json object of a DataMiningReportFieldFilter
	#' @param DataMiningReportFieldFilterID The ID of the DataMiningReportFieldFilter to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DataMiningReportFieldFilter. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DataMiningReportFieldFilter.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DataMiningReportFieldFilter') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of DataMiningReportFieldFilter
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDataMiningReportFieldFilter <- function(DataMiningReportFieldFilterID, FilterType = F, Low = F, High = F, ListBackingData = F, ListDisplayValue = F, CurrentUserCanRead = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SubtopicFieldID = F, DataMiningReportID = F, DistinctMultiSelectSchemaObject = F, DistinctMultiSelectModule = F, DistinctMultiSelectObject = F, DistinctMultiSelectFieldPath = F, FormatedDisplayValue = F, IsPrompt = F, PromptLabel = F, PromptIsRequired = F, ComparisonType = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DataMiningReportFieldFilterID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "DataMiningReportFieldFilter", objectId = DataMiningReportFieldFilterID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DataMiningReportFieldFilter
	#'
	#' This function deletes a DataMiningReportFieldFilter
	#' @param DataMiningReportFieldFilterID The ID of the DataMiningReportFieldFilter to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The DataMiningReportFieldFilterID of the deleted DataMiningReportFieldFilter.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDataMiningReportFieldFilter <- function(DataMiningReportFieldFilterID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "DataMiningReportFieldFilter", objectId = DataMiningReportFieldFilterID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DataMiningReportFieldFilter
	#'
	#' This function creates a DataMiningReportFieldFilter
	#' @param fieldNames The field values to give the created DataMiningReportFieldFilter. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created DataMiningReportFieldFilter
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDataMiningReportFieldFilter <- function(FilterType = NULL, Low = NULL, High = NULL, SubtopicFieldID = NULL, DataMiningReportID = NULL, IsPrompt = NULL, PromptLabel = NULL, PromptIsRequired = NULL, ComparisonType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "DataMiningReportFieldFilter", body = list(DataObject = body), searchFields = append("DataMiningReportFieldFilterID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DataMiningReportFieldFilter
	#'
	#' This function modifies a DataMiningReportFieldFilter
	#' @param fieldNames The field values to give the modified DataMiningReportFieldFilter. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified DataMiningReportFieldFilter
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDataMiningReportFieldFilter <- function(DataMiningReportFieldFilterID, FilterType = NULL, Low = NULL, High = NULL, SubtopicFieldID = NULL, DataMiningReportID = NULL, IsPrompt = NULL, PromptLabel = NULL, PromptIsRequired = NULL, ComparisonType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "DataMiningReportFieldFilter", objectId = DataMiningReportFieldFilterID, body = list(DataObject = body), searchFields = append("DataMiningReportFieldFilterID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DataMiningReportFieldSorts
	#'
	#' This function returns a dataframe or json object of DataMiningReportFieldSorts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DataMiningReportFieldSorts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DataMiningReportFieldSorts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DataMiningReportFieldSort') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of DataMiningReportFieldSorts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDataMiningReportFieldSorts <- function(searchConditionsList = NULL, DataMiningReportFieldSortID = F, SortDirection = F, SortOrder = F, ShowCount = F, CurrentUserCanRead = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SubtopicFieldID = F, DataMiningReportID = F, ShowSubtotals = F, BreakType = F, Length = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "DataMiningReportFieldSort", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DataMiningReportFieldSort
	#'
	#' This function returns a dataframe or json object of a DataMiningReportFieldSort
	#' @param DataMiningReportFieldSortID The ID of the DataMiningReportFieldSort to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DataMiningReportFieldSort. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DataMiningReportFieldSort.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DataMiningReportFieldSort') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of DataMiningReportFieldSort
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDataMiningReportFieldSort <- function(DataMiningReportFieldSortID, SortDirection = F, SortOrder = F, ShowCount = F, CurrentUserCanRead = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SubtopicFieldID = F, DataMiningReportID = F, ShowSubtotals = F, BreakType = F, Length = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DataMiningReportFieldSortID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "DataMiningReportFieldSort", objectId = DataMiningReportFieldSortID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DataMiningReportFieldSort
	#'
	#' This function deletes a DataMiningReportFieldSort
	#' @param DataMiningReportFieldSortID The ID of the DataMiningReportFieldSort to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The DataMiningReportFieldSortID of the deleted DataMiningReportFieldSort.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDataMiningReportFieldSort <- function(DataMiningReportFieldSortID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "DataMiningReportFieldSort", objectId = DataMiningReportFieldSortID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DataMiningReportFieldSort
	#'
	#' This function creates a DataMiningReportFieldSort
	#' @param fieldNames The field values to give the created DataMiningReportFieldSort. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created DataMiningReportFieldSort
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDataMiningReportFieldSort <- function(SortDirection = NULL, SortOrder = NULL, ShowCount = NULL, SubtopicFieldID = NULL, DataMiningReportID = NULL, ShowSubtotals = NULL, BreakType = NULL, Length = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "DataMiningReportFieldSort", body = list(DataObject = body), searchFields = append("DataMiningReportFieldSortID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DataMiningReportFieldSort
	#'
	#' This function modifies a DataMiningReportFieldSort
	#' @param fieldNames The field values to give the modified DataMiningReportFieldSort. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified DataMiningReportFieldSort
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDataMiningReportFieldSort <- function(DataMiningReportFieldSortID, SortDirection = NULL, SortOrder = NULL, ShowCount = NULL, SubtopicFieldID = NULL, DataMiningReportID = NULL, ShowSubtotals = NULL, BreakType = NULL, Length = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "DataMiningReportFieldSort", objectId = DataMiningReportFieldSortID, body = list(DataObject = body), searchFields = append("DataMiningReportFieldSortID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SubtopicFields
	#'
	#' This function returns a dataframe or json object of SubtopicFields
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SubtopicFields. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SubtopicFields.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SubtopicField') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of SubtopicFields
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSubtopicFields <- function(searchConditionsList = NULL, SubtopicFieldID = F, SubtopicID = F, FullFieldPath = F, SystemType = F, IsCalculatedInCSharp = F, IsFilterable = F, IsString = F, IsNumeric = F, IsDateTime = F, IsTimeSpan = F, IsBoolean = F, IsEnum = F, IsNotEnumOrBoolean = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, MaxSortBreakPosition = F, CurrentUserHasAccess = F, SkywardHash = F, SkywardID = F, IsSkywardMaintained = F, FieldIDSkySys = F, SkywardFriendlyName = F, FriendlyName = F, OverrideFriendlyName = F, SkywardDisplayLevelCode = F, DisplayLevelCode = F, OverrideDisplayLevel = F, GuidFieldPath = F, FieldPath = F, Field = F, IsFieldPathValid = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "SubtopicField", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SubtopicField
	#'
	#' This function returns a dataframe or json object of a SubtopicField
	#' @param SubtopicFieldID The ID of the SubtopicField to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SubtopicField. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SubtopicField.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SubtopicField') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of SubtopicField
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSubtopicField <- function(SubtopicFieldID, SubtopicID = F, FullFieldPath = F, SystemType = F, IsCalculatedInCSharp = F, IsFilterable = F, IsString = F, IsNumeric = F, IsDateTime = F, IsTimeSpan = F, IsBoolean = F, IsEnum = F, IsNotEnumOrBoolean = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, MaxSortBreakPosition = F, CurrentUserHasAccess = F, SkywardHash = F, SkywardID = F, IsSkywardMaintained = F, FieldIDSkySys = F, SkywardFriendlyName = F, FriendlyName = F, OverrideFriendlyName = F, SkywardDisplayLevelCode = F, DisplayLevelCode = F, OverrideDisplayLevel = F, GuidFieldPath = F, FieldPath = F, Field = F, IsFieldPathValid = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SubtopicFieldID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "SubtopicField", objectId = SubtopicFieldID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SubtopicField
	#'
	#' This function deletes a SubtopicField
	#' @param SubtopicFieldID The ID of the SubtopicField to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The SubtopicFieldID of the deleted SubtopicField.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSubtopicField <- function(SubtopicFieldID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "SubtopicField", objectId = SubtopicFieldID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SubtopicField
	#'
	#' This function creates a SubtopicField
	#' @param fieldNames The field values to give the created SubtopicField. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created SubtopicField
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSubtopicField <- function(SubtopicID = NULL, SystemType = NULL, IsCalculatedInCSharp = NULL, FieldIDSkySys = NULL, FriendlyName = NULL, OverrideFriendlyName = NULL, DisplayLevelCode = NULL, OverrideDisplayLevel = NULL, FieldPath = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "SubtopicField", body = list(DataObject = body), searchFields = append("SubtopicFieldID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SubtopicField
	#'
	#' This function modifies a SubtopicField
	#' @param fieldNames The field values to give the modified SubtopicField. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified SubtopicField
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySubtopicField <- function(SubtopicFieldID, SubtopicID = NULL, SystemType = NULL, IsCalculatedInCSharp = NULL, FieldIDSkySys = NULL, FriendlyName = NULL, OverrideFriendlyName = NULL, DisplayLevelCode = NULL, OverrideDisplayLevel = NULL, FieldPath = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "SubtopicField", objectId = SubtopicFieldID, body = list(DataObject = body), searchFields = append("SubtopicFieldID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List UploadReportLogs
	#'
	#' This function returns a dataframe or json object of UploadReportLogs
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given UploadReportLogs. Defaults to FALSE for all return fields which, for convenience, returns all fields for the UploadReportLogs.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('UploadReportLog') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of UploadReportLogs
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listUploadReportLogs <- function(searchConditionsList = NULL, UploadReportLogID = F, FileName = F, Result = F, Message = F, ReportID = F, LogID = F, WorkflowInstanceID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "UploadReportLog", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an UploadReportLog
	#'
	#' This function returns a dataframe or json object of an UploadReportLog
	#' @param UploadReportLogID The ID of the UploadReportLog to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given UploadReportLog. Defaults to FALSE for all return fields which, for convenience, returns all fields for the UploadReportLog.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('UploadReportLog') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of UploadReportLog
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getUploadReportLog <- function(UploadReportLogID, FileName = F, Result = F, Message = F, ReportID = F, LogID = F, WorkflowInstanceID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "UploadReportLogID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "UploadReportLog", objectId = UploadReportLogID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an UploadReportLog
	#'
	#' This function deletes an UploadReportLog
	#' @param UploadReportLogID The ID of the UploadReportLog to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The UploadReportLogID of the deleted UploadReportLog.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteUploadReportLog <- function(UploadReportLogID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "UploadReportLog", objectId = UploadReportLogID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an UploadReportLog
	#'
	#' This function creates an UploadReportLog
	#' @param fieldNames The field values to give the created UploadReportLog. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created UploadReportLog
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createUploadReportLog <- function(FileName = NULL, Result = NULL, Message = NULL, ReportID = NULL, LogID = NULL, WorkflowInstanceID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "UploadReportLog", body = list(DataObject = body), searchFields = append("UploadReportLogID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an UploadReportLog
	#'
	#' This function modifies an UploadReportLog
	#' @param fieldNames The field values to give the modified UploadReportLog. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified UploadReportLog
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyUploadReportLog <- function(UploadReportLogID, FileName = NULL, Result = NULL, Message = NULL, ReportID = NULL, LogID = NULL, WorkflowInstanceID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "UploadReportLog", objectId = UploadReportLogID, body = list(DataObject = body), searchFields = append("UploadReportLogID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ReportingSubjects
	#'
	#' This function returns a dataframe or json object of ReportingSubjects
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReportingSubjects. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReportingSubjects.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReportingSubject') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of ReportingSubjects
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listReportingSubjects <- function(searchConditionsList = NULL, SubjectID = F, Name = F, ObjectID = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CurrentUserHasAccess = F, AllowAccountBreaks = F, PromptForFiscalYear = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "Subject", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ReportingSubject
	#'
	#' This function returns a dataframe or json object of a ReportingSubject
	#' @param ReportingSubjectID The ID of the ReportingSubject to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReportingSubject. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReportingSubject.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReportingSubject') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of ReportingSubject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getReportingSubject <- function(ReportingSubjectID, SubjectID = F, Name = F, ObjectID = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CurrentUserHasAccess = F, AllowAccountBreaks = F, PromptForFiscalYear = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ReportingSubjectID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "Subject", objectId = ReportingSubjectID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ReportingSubject
	#'
	#' This function deletes a ReportingSubject
	#' @param ReportingSubjectID The ID of the ReportingSubject to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The ReportingSubjectID of the deleted ReportingSubject.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteReportingSubject <- function(ReportingSubjectID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "Subject", objectId = ReportingSubjectID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ReportingSubject
	#'
	#' This function creates a ReportingSubject
	#' @param fieldNames The field values to give the created ReportingSubject. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created ReportingSubject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createReportingSubject <- function(Name = NULL, ObjectID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "Subject", body = list(DataObject = body), searchFields = append("SubjectID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ReportingSubject
	#'
	#' This function modifies a ReportingSubject
	#' @param fieldNames The field values to give the modified ReportingSubject. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified ReportingSubject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyReportingSubject <- function(SubjectID, Name = NULL, ObjectID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "Subject", objectId = SubjectID, body = list(DataObject = body), searchFields = append("SubjectID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DataMiningReportSubtopics
	#'
	#' This function returns a dataframe or json object of DataMiningReportSubtopics
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DataMiningReportSubtopics. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DataMiningReportSubtopics.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DataMiningReportSubtopic') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of DataMiningReportSubtopics
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDataMiningReportSubtopics <- function(searchConditionsList = NULL, DataMiningReportSubtopicID = F, DataMiningReportID = F, SubtopicID = F, HasRecord = F, HasRecordIsEditable = F, CurrentUserCanRead = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ShowSubtotals = F, OnlyShowTotals = F, ShowCount = F, SingleLineRecordCount = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "DataMiningReportSubtopic", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DataMiningReportSubtopic
	#'
	#' This function returns a dataframe or json object of a DataMiningReportSubtopic
	#' @param DataMiningReportSubtopicID The ID of the DataMiningReportSubtopic to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DataMiningReportSubtopic. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DataMiningReportSubtopic.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DataMiningReportSubtopic') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of DataMiningReportSubtopic
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDataMiningReportSubtopic <- function(DataMiningReportSubtopicID, DataMiningReportID = F, SubtopicID = F, HasRecord = F, HasRecordIsEditable = F, CurrentUserCanRead = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ShowSubtotals = F, OnlyShowTotals = F, ShowCount = F, SingleLineRecordCount = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DataMiningReportSubtopicID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "DataMiningReportSubtopic", objectId = DataMiningReportSubtopicID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DataMiningReportSubtopic
	#'
	#' This function deletes a DataMiningReportSubtopic
	#' @param DataMiningReportSubtopicID The ID of the DataMiningReportSubtopic to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The DataMiningReportSubtopicID of the deleted DataMiningReportSubtopic.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDataMiningReportSubtopic <- function(DataMiningReportSubtopicID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "DataMiningReportSubtopic", objectId = DataMiningReportSubtopicID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DataMiningReportSubtopic
	#'
	#' This function creates a DataMiningReportSubtopic
	#' @param fieldNames The field values to give the created DataMiningReportSubtopic. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created DataMiningReportSubtopic
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDataMiningReportSubtopic <- function(DataMiningReportID = NULL, SubtopicID = NULL, HasRecord = NULL, ShowSubtotals = NULL, OnlyShowTotals = NULL, ShowCount = NULL, SingleLineRecordCount = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "DataMiningReportSubtopic", body = list(DataObject = body), searchFields = append("DataMiningReportSubtopicID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DataMiningReportSubtopic
	#'
	#' This function modifies a DataMiningReportSubtopic
	#' @param fieldNames The field values to give the modified DataMiningReportSubtopic. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified DataMiningReportSubtopic
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDataMiningReportSubtopic <- function(DataMiningReportSubtopicID, DataMiningReportID = NULL, SubtopicID = NULL, HasRecord = NULL, ShowSubtotals = NULL, OnlyShowTotals = NULL, ShowCount = NULL, SingleLineRecordCount = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "DataMiningReportSubtopic", objectId = DataMiningReportSubtopicID, body = list(DataObject = body), searchFields = append("DataMiningReportSubtopicID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PromptTemplates
	#'
	#' This function returns a dataframe or json object of PromptTemplates
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PromptTemplates. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PromptTemplates.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PromptTemplate') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of PromptTemplates
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPromptTemplates <- function(searchConditionsList = NULL, PromptTemplateID = F, EntityID = F, UserIDOwner = F, ReportID = F, Name = F, Description = F, PromptXML = F, PromptValues = F, Published = F, CurrentUserIsOwnerOrCreator = F, CurrentUserIsOwnerOrCreatorNonOperation = F, ModifiedDate = F, CurrentUserHasAccess = F, ReportDefinitionXml = F, ReportDefinition = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "PromptTemplate", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PromptTemplate
	#'
	#' This function returns a dataframe or json object of a PromptTemplate
	#' @param PromptTemplateID The ID of the PromptTemplate to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PromptTemplate. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PromptTemplate.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PromptTemplate') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of PromptTemplate
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPromptTemplate <- function(PromptTemplateID, EntityID = F, UserIDOwner = F, ReportID = F, Name = F, Description = F, PromptXML = F, PromptValues = F, Published = F, CurrentUserIsOwnerOrCreator = F, CurrentUserIsOwnerOrCreatorNonOperation = F, ModifiedDate = F, CurrentUserHasAccess = F, ReportDefinitionXml = F, ReportDefinition = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PromptTemplateID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "PromptTemplate", objectId = PromptTemplateID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PromptTemplate
	#'
	#' This function deletes a PromptTemplate
	#' @param PromptTemplateID The ID of the PromptTemplate to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The PromptTemplateID of the deleted PromptTemplate.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePromptTemplate <- function(PromptTemplateID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "PromptTemplate", objectId = PromptTemplateID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PromptTemplate
	#'
	#' This function creates a PromptTemplate
	#' @param fieldNames The field values to give the created PromptTemplate. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created PromptTemplate
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPromptTemplate <- function(EntityID = NULL, UserIDOwner = NULL, ReportID = NULL, Name = NULL, Description = NULL, Published = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "PromptTemplate", body = list(DataObject = body), searchFields = append("PromptTemplateID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PromptTemplate
	#'
	#' This function modifies a PromptTemplate
	#' @param fieldNames The field values to give the modified PromptTemplate. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified PromptTemplate
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPromptTemplate <- function(PromptTemplateID, EntityID = NULL, UserIDOwner = NULL, ReportID = NULL, Name = NULL, Description = NULL, Published = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "PromptTemplate", objectId = PromptTemplateID, body = list(DataObject = body), searchFields = append("PromptTemplateID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SubjectSecurityLocations
	#'
	#' This function returns a dataframe or json object of SubjectSecurityLocations
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SubjectSecurityLocations. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SubjectSecurityLocations.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SubjectSecurityLocation') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of SubjectSecurityLocations
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSubjectSecurityLocations <- function(searchConditionsList = NULL, SubjectSecurityLocationID = F, SkywardID = F, SubjectID = F, SecurityLocationID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "SubjectSecurityLocation", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SubjectSecurityLocation
	#'
	#' This function returns a dataframe or json object of a SubjectSecurityLocation
	#' @param SubjectSecurityLocationID The ID of the SubjectSecurityLocation to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SubjectSecurityLocation. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SubjectSecurityLocation.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SubjectSecurityLocation') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of SubjectSecurityLocation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSubjectSecurityLocation <- function(SubjectSecurityLocationID, SkywardID = F, SubjectID = F, SecurityLocationID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SubjectSecurityLocationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "SubjectSecurityLocation", objectId = SubjectSecurityLocationID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SubjectSecurityLocation
	#'
	#' This function deletes a SubjectSecurityLocation
	#' @param SubjectSecurityLocationID The ID of the SubjectSecurityLocation to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The SubjectSecurityLocationID of the deleted SubjectSecurityLocation.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSubjectSecurityLocation <- function(SubjectSecurityLocationID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "SubjectSecurityLocation", objectId = SubjectSecurityLocationID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SubjectSecurityLocation
	#'
	#' This function creates a SubjectSecurityLocation
	#' @param fieldNames The field values to give the created SubjectSecurityLocation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created SubjectSecurityLocation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSubjectSecurityLocation <- function(SubjectID = NULL, SecurityLocationID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "SubjectSecurityLocation", body = list(DataObject = body), searchFields = append("SubjectSecurityLocationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SubjectSecurityLocation
	#'
	#' This function modifies a SubjectSecurityLocation
	#' @param fieldNames The field values to give the modified SubjectSecurityLocation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified SubjectSecurityLocation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySubjectSecurityLocation <- function(SubjectSecurityLocationID, SubjectID = NULL, SecurityLocationID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "SubjectSecurityLocation", objectId = SubjectSecurityLocationID, body = list(DataObject = body), searchFields = append("SubjectSecurityLocationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DataMiningReportSortBreaks
	#'
	#' This function returns a dataframe or json object of DataMiningReportSortBreaks
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DataMiningReportSortBreaks. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DataMiningReportSortBreaks.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DataMiningReportSortBreak') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of DataMiningReportSortBreaks
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDataMiningReportSortBreaks <- function(searchConditionsList = NULL, DataMiningReportSortBreakID = F, DataMiningReportFieldSortID = F, SortBreakID = F, BreakType = F, HasSeparator = F, HasDoubleUnderline = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "DataMiningReportSortBreak", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DataMiningReportSortBreak
	#'
	#' This function returns a dataframe or json object of a DataMiningReportSortBreak
	#' @param DataMiningReportSortBreakID The ID of the DataMiningReportSortBreak to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DataMiningReportSortBreak. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DataMiningReportSortBreak.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DataMiningReportSortBreak') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of DataMiningReportSortBreak
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDataMiningReportSortBreak <- function(DataMiningReportSortBreakID, DataMiningReportFieldSortID = F, SortBreakID = F, BreakType = F, HasSeparator = F, HasDoubleUnderline = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DataMiningReportSortBreakID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "DataMiningReportSortBreak", objectId = DataMiningReportSortBreakID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DataMiningReportSortBreak
	#'
	#' This function deletes a DataMiningReportSortBreak
	#' @param DataMiningReportSortBreakID The ID of the DataMiningReportSortBreak to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The DataMiningReportSortBreakID of the deleted DataMiningReportSortBreak.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDataMiningReportSortBreak <- function(DataMiningReportSortBreakID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "DataMiningReportSortBreak", objectId = DataMiningReportSortBreakID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DataMiningReportSortBreak
	#'
	#' This function creates a DataMiningReportSortBreak
	#' @param fieldNames The field values to give the created DataMiningReportSortBreak. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created DataMiningReportSortBreak
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDataMiningReportSortBreak <- function(DataMiningReportFieldSortID = NULL, SortBreakID = NULL, BreakType = NULL, HasSeparator = NULL, HasDoubleUnderline = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "DataMiningReportSortBreak", body = list(DataObject = body), searchFields = append("DataMiningReportSortBreakID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DataMiningReportSortBreak
	#'
	#' This function modifies a DataMiningReportSortBreak
	#' @param fieldNames The field values to give the modified DataMiningReportSortBreak. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified DataMiningReportSortBreak
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDataMiningReportSortBreak <- function(DataMiningReportSortBreakID, DataMiningReportFieldSortID = NULL, SortBreakID = NULL, BreakType = NULL, HasSeparator = NULL, HasDoubleUnderline = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "DataMiningReportSortBreak", objectId = DataMiningReportSortBreakID, body = list(DataObject = body), searchFields = append("DataMiningReportSortBreakID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SortBreaks
	#'
	#' This function returns a dataframe or json object of SortBreaks
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SortBreaks. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SortBreaks.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SortBreak') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of SortBreaks
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSortBreaks <- function(searchConditionsList = NULL, SortBreakID = F, SkywardHash = F, SkywardID = F, CharacterPosition = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SubtopicFieldID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "SortBreak", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SortBreak
	#'
	#' This function returns a dataframe or json object of a SortBreak
	#' @param SortBreakID The ID of the SortBreak to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SortBreak. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SortBreak.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SortBreak') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of SortBreak
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSortBreak <- function(SortBreakID, SkywardHash = F, SkywardID = F, CharacterPosition = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SubtopicFieldID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SortBreakID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "SortBreak", objectId = SortBreakID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SortBreak
	#'
	#' This function deletes a SortBreak
	#' @param SortBreakID The ID of the SortBreak to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The SortBreakID of the deleted SortBreak.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSortBreak <- function(SortBreakID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "SortBreak", objectId = SortBreakID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SortBreak
	#'
	#' This function creates a SortBreak
	#' @param fieldNames The field values to give the created SortBreak. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created SortBreak
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSortBreak <- function(CharacterPosition = NULL, SubtopicFieldID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "SortBreak", body = list(DataObject = body), searchFields = append("SortBreakID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SortBreak
	#'
	#' This function modifies a SortBreak
	#' @param fieldNames The field values to give the modified SortBreak. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified SortBreak
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySortBreak <- function(SortBreakID, CharacterPosition = NULL, SubtopicFieldID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "SortBreak", objectId = SortBreakID, body = list(DataObject = body), searchFields = append("SortBreakID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Sorts
	#'
	#' This function returns a dataframe or json object of Sorts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Sorts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Sorts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Sort') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of Sorts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSorts <- function(searchConditionsList = NULL, SortID = F, SortGroupID = F, SubtopicFieldID = F, SortOrder = F, SortDirectionCode = F, SkywardHash = F, SkywardID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "Sort", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Sort
	#'
	#' This function returns a dataframe or json object of a Sort
	#' @param SortID The ID of the Sort to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Sort. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Sort.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Sort') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of Sort
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSort <- function(SortID, SortGroupID = F, SubtopicFieldID = F, SortOrder = F, SortDirectionCode = F, SkywardHash = F, SkywardID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SortID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "Sort", objectId = SortID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Sort
	#'
	#' This function deletes a Sort
	#' @param SortID The ID of the Sort to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The SortID of the deleted Sort.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSort <- function(SortID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "Sort", objectId = SortID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Sort
	#'
	#' This function creates a Sort
	#' @param fieldNames The field values to give the created Sort. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created Sort
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSort <- function(SortGroupID = NULL, SubtopicFieldID = NULL, SortOrder = NULL, SortDirectionCode = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "Sort", body = list(DataObject = body), searchFields = append("SortID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Sort
	#'
	#' This function modifies a Sort
	#' @param fieldNames The field values to give the modified Sort. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified Sort
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySort <- function(SortID, SortGroupID = NULL, SubtopicFieldID = NULL, SortOrder = NULL, SortDirectionCode = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "Sort", objectId = SortID, body = list(DataObject = body), searchFields = append("SortID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SortGroups
	#'
	#' This function returns a dataframe or json object of SortGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SortGroups. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SortGroups.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SortGroup') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of SortGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSortGroups <- function(searchConditionsList = NULL, SortGroupID = F, ObjectID = F, IsDefault = F, Name = F, SkywardHash = F, SkywardID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "SortGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SortGroup
	#'
	#' This function returns a dataframe or json object of a SortGroup
	#' @param SortGroupID The ID of the SortGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SortGroup. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SortGroup.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SortGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of SortGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSortGroup <- function(SortGroupID, ObjectID = F, IsDefault = F, Name = F, SkywardHash = F, SkywardID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SortGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "SortGroup", objectId = SortGroupID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SortGroup
	#'
	#' This function deletes a SortGroup
	#' @param SortGroupID The ID of the SortGroup to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The SortGroupID of the deleted SortGroup.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSortGroup <- function(SortGroupID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "SortGroup", objectId = SortGroupID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SortGroup
	#'
	#' This function creates a SortGroup
	#' @param fieldNames The field values to give the created SortGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created SortGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSortGroup <- function(ObjectID = NULL, IsDefault = NULL, Name = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "SortGroup", body = list(DataObject = body), searchFields = append("SortGroupID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SortGroup
	#'
	#' This function modifies a SortGroup
	#' @param fieldNames The field values to give the modified SortGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified SortGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySortGroup <- function(SortGroupID, ObjectID = NULL, IsDefault = NULL, Name = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "SortGroup", objectId = SortGroupID, body = list(DataObject = body), searchFields = append("SortGroupID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PageBursts
	#'
	#' This function returns a dataframe or json object of PageBursts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PageBursts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PageBursts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PageBurst') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of PageBursts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPageBursts <- function(searchConditionsList = NULL, PageBurstID = F, ObjectID = F, ObjectPrimaryKey = F, PageID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "PageBurst", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PageBurst
	#'
	#' This function returns a dataframe or json object of a PageBurst
	#' @param PageBurstID The ID of the PageBurst to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PageBurst. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PageBurst.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PageBurst') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of PageBurst
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPageBurst <- function(PageBurstID, ObjectID = F, ObjectPrimaryKey = F, PageID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PageBurstID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "PageBurst", objectId = PageBurstID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PageBurst
	#'
	#' This function deletes a PageBurst
	#' @param PageBurstID The ID of the PageBurst to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The PageBurstID of the deleted PageBurst.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePageBurst <- function(PageBurstID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "PageBurst", objectId = PageBurstID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PageBurst
	#'
	#' This function creates a PageBurst
	#' @param fieldNames The field values to give the created PageBurst. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created PageBurst
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPageBurst <- function(ObjectID = NULL, ObjectPrimaryKey = NULL, PageID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "PageBurst", body = list(DataObject = body), searchFields = append("PageBurstID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PageBurst
	#'
	#' This function modifies a PageBurst
	#' @param fieldNames The field values to give the modified PageBurst. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified PageBurst
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPageBurst <- function(PageBurstID, ObjectID = NULL, ObjectPrimaryKey = NULL, PageID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "PageBurst", objectId = PageBurstID, body = list(DataObject = body), searchFields = append("PageBurstID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List BurstActions
	#'
	#' This function returns a dataframe or json object of BurstActions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BurstActions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BurstActions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BurstAction') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of BurstActions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listBurstActions <- function(searchConditionsList = NULL, BurstActionID = F, SkywardID = F, SkywardHash = F, ActionHandler = F, Name = F, ObjectID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ParameterSxlPath = F, IsPrintAction = F, CanBeReverted = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "BurstAction", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a BurstAction
	#'
	#' This function returns a dataframe or json object of a BurstAction
	#' @param BurstActionID The ID of the BurstAction to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BurstAction. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BurstAction.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BurstAction') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of BurstAction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getBurstAction <- function(BurstActionID, SkywardID = F, SkywardHash = F, ActionHandler = F, Name = F, ObjectID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ParameterSxlPath = F, IsPrintAction = F, CanBeReverted = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "BurstActionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "BurstAction", objectId = BurstActionID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a BurstAction
	#'
	#' This function deletes a BurstAction
	#' @param BurstActionID The ID of the BurstAction to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The BurstActionID of the deleted BurstAction.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteBurstAction <- function(BurstActionID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "BurstAction", objectId = BurstActionID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a BurstAction
	#'
	#' This function creates a BurstAction
	#' @param fieldNames The field values to give the created BurstAction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created BurstAction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createBurstAction <- function(Name = NULL, ObjectID = NULL, IsPrintAction = NULL, CanBeReverted = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "BurstAction", body = list(DataObject = body), searchFields = append("BurstActionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a BurstAction
	#'
	#' This function modifies a BurstAction
	#' @param fieldNames The field values to give the modified BurstAction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified BurstAction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyBurstAction <- function(BurstActionID, Name = NULL, ObjectID = NULL, IsPrintAction = NULL, CanBeReverted = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "BurstAction", objectId = BurstActionID, body = list(DataObject = body), searchFields = append("BurstActionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ReportQueueBurstActions
	#'
	#' This function returns a dataframe or json object of ReportQueueBurstActions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReportQueueBurstActions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReportQueueBurstActions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReportQueueBurstAction') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of ReportQueueBurstActions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listReportQueueBurstActions <- function(searchConditionsList = NULL, ReportQueueBurstActionID = F, ReportQueueID = F, BurstActionID = F, StatusCode = F, Status = F, Parameters = F, LogID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ChangesetXML = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "ReportQueueBurstAction", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ReportQueueBurstAction
	#'
	#' This function returns a dataframe or json object of a ReportQueueBurstAction
	#' @param ReportQueueBurstActionID The ID of the ReportQueueBurstAction to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReportQueueBurstAction. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReportQueueBurstAction.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReportQueueBurstAction') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of ReportQueueBurstAction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getReportQueueBurstAction <- function(ReportQueueBurstActionID, ReportQueueID = F, BurstActionID = F, StatusCode = F, Status = F, Parameters = F, LogID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ChangesetXML = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ReportQueueBurstActionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "ReportQueueBurstAction", objectId = ReportQueueBurstActionID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ReportQueueBurstAction
	#'
	#' This function deletes a ReportQueueBurstAction
	#' @param ReportQueueBurstActionID The ID of the ReportQueueBurstAction to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The ReportQueueBurstActionID of the deleted ReportQueueBurstAction.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteReportQueueBurstAction <- function(ReportQueueBurstActionID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "ReportQueueBurstAction", objectId = ReportQueueBurstActionID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ReportQueueBurstAction
	#'
	#' This function creates a ReportQueueBurstAction
	#' @param fieldNames The field values to give the created ReportQueueBurstAction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created ReportQueueBurstAction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createReportQueueBurstAction <- function(ReportQueueID = NULL, BurstActionID = NULL, Status = NULL, LogID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "ReportQueueBurstAction", body = list(DataObject = body), searchFields = append("ReportQueueBurstActionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ReportQueueBurstAction
	#'
	#' This function modifies a ReportQueueBurstAction
	#' @param fieldNames The field values to give the modified ReportQueueBurstAction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified ReportQueueBurstAction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyReportQueueBurstAction <- function(ReportQueueBurstActionID, ReportQueueID = NULL, BurstActionID = NULL, Status = NULL, LogID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "ReportQueueBurstAction", objectId = ReportQueueBurstActionID, body = list(DataObject = body), searchFields = append("ReportQueueBurstActionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SummaryReportParameters
	#'
	#' This function returns a dataframe or json object of SummaryReportParameters
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SummaryReportParameters. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SummaryReportParameters.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SummaryReportParameter') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of SummaryReportParameters
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSummaryReportParameters <- function(searchConditionsList = NULL, SummaryReportParameterID = F, Name = F, Value = F, UsedBy = F, ReportQueueID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "SummaryReportParameter", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SummaryReportParameter
	#'
	#' This function returns a dataframe or json object of a SummaryReportParameter
	#' @param SummaryReportParameterID The ID of the SummaryReportParameter to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SummaryReportParameter. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SummaryReportParameter.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SummaryReportParameter') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of SummaryReportParameter
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSummaryReportParameter <- function(SummaryReportParameterID, Name = F, Value = F, UsedBy = F, ReportQueueID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SummaryReportParameterID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "SummaryReportParameter", objectId = SummaryReportParameterID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SummaryReportParameter
	#'
	#' This function deletes a SummaryReportParameter
	#' @param SummaryReportParameterID The ID of the SummaryReportParameter to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The SummaryReportParameterID of the deleted SummaryReportParameter.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSummaryReportParameter <- function(SummaryReportParameterID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "SummaryReportParameter", objectId = SummaryReportParameterID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SummaryReportParameter
	#'
	#' This function creates a SummaryReportParameter
	#' @param fieldNames The field values to give the created SummaryReportParameter. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created SummaryReportParameter
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSummaryReportParameter <- function(Name = NULL, Value = NULL, UsedBy = NULL, ReportQueueID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "SummaryReportParameter", body = list(DataObject = body), searchFields = append("SummaryReportParameterID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SummaryReportParameter
	#'
	#' This function modifies a SummaryReportParameter
	#' @param fieldNames The field values to give the modified SummaryReportParameter. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified SummaryReportParameter
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySummaryReportParameter <- function(SummaryReportParameterID, Name = NULL, Value = NULL, UsedBy = NULL, ReportQueueID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "SummaryReportParameter", objectId = SummaryReportParameterID, body = list(DataObject = body), searchFields = append("SummaryReportParameterID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SummaryReportPrompts
	#'
	#' This function returns a dataframe or json object of SummaryReportPrompts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SummaryReportPrompts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SummaryReportPrompts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SummaryReportPrompt') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of SummaryReportPrompts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSummaryReportPrompts <- function(searchConditionsList = NULL, SummaryReportPromptID = F, ReportQueueID = F, Label = F, Value = F, Order = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "SummaryReportPrompt", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SummaryReportPrompt
	#'
	#' This function returns a dataframe or json object of a SummaryReportPrompt
	#' @param SummaryReportPromptID The ID of the SummaryReportPrompt to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SummaryReportPrompt. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SummaryReportPrompt.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SummaryReportPrompt') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of SummaryReportPrompt
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSummaryReportPrompt <- function(SummaryReportPromptID, ReportQueueID = F, Label = F, Value = F, Order = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SummaryReportPromptID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "SummaryReportPrompt", objectId = SummaryReportPromptID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SummaryReportPrompt
	#'
	#' This function deletes a SummaryReportPrompt
	#' @param SummaryReportPromptID The ID of the SummaryReportPrompt to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The SummaryReportPromptID of the deleted SummaryReportPrompt.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSummaryReportPrompt <- function(SummaryReportPromptID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "SummaryReportPrompt", objectId = SummaryReportPromptID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SummaryReportPrompt
	#'
	#' This function creates a SummaryReportPrompt
	#' @param fieldNames The field values to give the created SummaryReportPrompt. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created SummaryReportPrompt
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSummaryReportPrompt <- function(ReportQueueID = NULL, Label = NULL, Value = NULL, Order = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "SummaryReportPrompt", body = list(DataObject = body), searchFields = append("SummaryReportPromptID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SummaryReportPrompt
	#'
	#' This function modifies a SummaryReportPrompt
	#' @param fieldNames The field values to give the modified SummaryReportPrompt. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified SummaryReportPrompt
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySummaryReportPrompt <- function(SummaryReportPromptID, ReportQueueID = NULL, Label = NULL, Value = NULL, Order = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "SummaryReportPrompt", objectId = SummaryReportPromptID, body = list(DataObject = body), searchFields = append("SummaryReportPromptID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SummaryReportSections
	#'
	#' This function returns a dataframe or json object of SummaryReportSections
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SummaryReportSections. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SummaryReportSections.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SummaryReportSection') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of SummaryReportSections
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSummaryReportSections <- function(searchConditionsList = NULL, SummaryReportSectionID = F, ReportQueueID = F, Path = F, DisplayName = F, Order = F, HiddenType = F, HiddenTypeCode = F, SummaryReportSectionIDParent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "SummaryReportSection", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SummaryReportSection
	#'
	#' This function returns a dataframe or json object of a SummaryReportSection
	#' @param SummaryReportSectionID The ID of the SummaryReportSection to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SummaryReportSection. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SummaryReportSection.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SummaryReportSection') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of SummaryReportSection
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSummaryReportSection <- function(SummaryReportSectionID, ReportQueueID = F, Path = F, DisplayName = F, Order = F, HiddenType = F, HiddenTypeCode = F, SummaryReportSectionIDParent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SummaryReportSectionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "SummaryReportSection", objectId = SummaryReportSectionID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SummaryReportSection
	#'
	#' This function deletes a SummaryReportSection
	#' @param SummaryReportSectionID The ID of the SummaryReportSection to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The SummaryReportSectionID of the deleted SummaryReportSection.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSummaryReportSection <- function(SummaryReportSectionID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "SummaryReportSection", objectId = SummaryReportSectionID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SummaryReportSection
	#'
	#' This function creates a SummaryReportSection
	#' @param fieldNames The field values to give the created SummaryReportSection. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created SummaryReportSection
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSummaryReportSection <- function(ReportQueueID = NULL, Path = NULL, DisplayName = NULL, Order = NULL, HiddenType = NULL, SummaryReportSectionIDParent = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "SummaryReportSection", body = list(DataObject = body), searchFields = append("SummaryReportSectionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SummaryReportSection
	#'
	#' This function modifies a SummaryReportSection
	#' @param fieldNames The field values to give the modified SummaryReportSection. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified SummaryReportSection
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySummaryReportSection <- function(SummaryReportSectionID, ReportQueueID = NULL, Path = NULL, DisplayName = NULL, Order = NULL, HiddenType = NULL, SummaryReportSectionIDParent = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "SummaryReportSection", objectId = SummaryReportSectionID, body = list(DataObject = body), searchFields = append("SummaryReportSectionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SummaryReportSectionColumns
	#'
	#' This function returns a dataframe or json object of SummaryReportSectionColumns
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SummaryReportSectionColumns. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SummaryReportSectionColumns.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SummaryReportSectionColumn') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of SummaryReportSectionColumns
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSummaryReportSectionColumns <- function(searchConditionsList = NULL, SummaryReportSectionColumnID = F, SummaryReportSectionID = F, DisplayName = F, FieldName = F, DataType = F, HiddenType = F, HiddenTypeCode = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "SummaryReportSectionColumn", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SummaryReportSectionColumn
	#'
	#' This function returns a dataframe or json object of a SummaryReportSectionColumn
	#' @param SummaryReportSectionColumnID The ID of the SummaryReportSectionColumn to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SummaryReportSectionColumn. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SummaryReportSectionColumn.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SummaryReportSectionColumn') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of SummaryReportSectionColumn
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSummaryReportSectionColumn <- function(SummaryReportSectionColumnID, SummaryReportSectionID = F, DisplayName = F, FieldName = F, DataType = F, HiddenType = F, HiddenTypeCode = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SummaryReportSectionColumnID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "SummaryReportSectionColumn", objectId = SummaryReportSectionColumnID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SummaryReportSectionColumn
	#'
	#' This function deletes a SummaryReportSectionColumn
	#' @param SummaryReportSectionColumnID The ID of the SummaryReportSectionColumn to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The SummaryReportSectionColumnID of the deleted SummaryReportSectionColumn.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSummaryReportSectionColumn <- function(SummaryReportSectionColumnID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "SummaryReportSectionColumn", objectId = SummaryReportSectionColumnID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SummaryReportSectionColumn
	#'
	#' This function creates a SummaryReportSectionColumn
	#' @param fieldNames The field values to give the created SummaryReportSectionColumn. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created SummaryReportSectionColumn
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSummaryReportSectionColumn <- function(SummaryReportSectionID = NULL, DisplayName = NULL, FieldName = NULL, DataType = NULL, HiddenType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "SummaryReportSectionColumn", body = list(DataObject = body), searchFields = append("SummaryReportSectionColumnID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SummaryReportSectionColumn
	#'
	#' This function modifies a SummaryReportSectionColumn
	#' @param fieldNames The field values to give the modified SummaryReportSectionColumn. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified SummaryReportSectionColumn
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySummaryReportSectionColumn <- function(SummaryReportSectionColumnID, SummaryReportSectionID = NULL, DisplayName = NULL, FieldName = NULL, DataType = NULL, HiddenType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "SummaryReportSectionColumn", objectId = SummaryReportSectionColumnID, body = list(DataObject = body), searchFields = append("SummaryReportSectionColumnID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ReportingConfigSystems
	#'
	#' This function returns a dataframe or json object of ReportingConfigSystems
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReportingConfigSystems. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReportingConfigSystems.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReportingConfigSystem') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of ReportingConfigSystems
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listReportingConfigSystems <- function(searchConditionsList = NULL, ConfigSystemID = F, DaysToSaveReportResults = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "ConfigSystem", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ReportingConfigSystem
	#'
	#' This function returns a dataframe or json object of a ReportingConfigSystem
	#' @param ReportingConfigSystemID The ID of the ReportingConfigSystem to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReportingConfigSystem. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReportingConfigSystem.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReportingConfigSystem') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of ReportingConfigSystem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getReportingConfigSystem <- function(ReportingConfigSystemID, ConfigSystemID = F, DaysToSaveReportResults = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ReportingConfigSystemID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "ConfigSystem", objectId = ReportingConfigSystemID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ReportingConfigSystem
	#'
	#' This function deletes a ReportingConfigSystem
	#' @param ReportingConfigSystemID The ID of the ReportingConfigSystem to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The ReportingConfigSystemID of the deleted ReportingConfigSystem.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteReportingConfigSystem <- function(ReportingConfigSystemID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "ConfigSystem", objectId = ReportingConfigSystemID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ReportingConfigSystem
	#'
	#' This function creates a ReportingConfigSystem
	#' @param fieldNames The field values to give the created ReportingConfigSystem. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created ReportingConfigSystem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createReportingConfigSystem <- function(DaysToSaveReportResults = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "ConfigSystem", body = list(DataObject = body), searchFields = append("ConfigSystemID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ReportingConfigSystem
	#'
	#' This function modifies a ReportingConfigSystem
	#' @param fieldNames The field values to give the modified ReportingConfigSystem. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified ReportingConfigSystem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyReportingConfigSystem <- function(ConfigSystemID, DaysToSaveReportResults = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "ConfigSystem", objectId = ConfigSystemID, body = list(DataObject = body), searchFields = append("ConfigSystemID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DataMiningReportSubtopicStandardFilters
	#'
	#' This function returns a dataframe or json object of DataMiningReportSubtopicStandardFilters
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DataMiningReportSubtopicStandardFilters. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DataMiningReportSubtopicStandardFilters.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DataMiningReportSubtopicStandardFilter') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of DataMiningReportSubtopicStandardFilters
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDataMiningReportSubtopicStandardFilters <- function(searchConditionsList = NULL, DataMiningReportSubtopicStandardFilterID = F, IsEnabled = F, DataMiningReportID = F, SubtopicStandardFilterID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "DataMiningReportSubtopicStandardFilter", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DataMiningReportSubtopicStandardFilter
	#'
	#' This function returns a dataframe or json object of a DataMiningReportSubtopicStandardFilter
	#' @param DataMiningReportSubtopicStandardFilterID The ID of the DataMiningReportSubtopicStandardFilter to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DataMiningReportSubtopicStandardFilter. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DataMiningReportSubtopicStandardFilter.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DataMiningReportSubtopicStandardFilter') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of DataMiningReportSubtopicStandardFilter
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDataMiningReportSubtopicStandardFilter <- function(DataMiningReportSubtopicStandardFilterID, IsEnabled = F, DataMiningReportID = F, SubtopicStandardFilterID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DataMiningReportSubtopicStandardFilterID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "DataMiningReportSubtopicStandardFilter", objectId = DataMiningReportSubtopicStandardFilterID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DataMiningReportSubtopicStandardFilter
	#'
	#' This function deletes a DataMiningReportSubtopicStandardFilter
	#' @param DataMiningReportSubtopicStandardFilterID The ID of the DataMiningReportSubtopicStandardFilter to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The DataMiningReportSubtopicStandardFilterID of the deleted DataMiningReportSubtopicStandardFilter.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDataMiningReportSubtopicStandardFilter <- function(DataMiningReportSubtopicStandardFilterID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "DataMiningReportSubtopicStandardFilter", objectId = DataMiningReportSubtopicStandardFilterID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DataMiningReportSubtopicStandardFilter
	#'
	#' This function creates a DataMiningReportSubtopicStandardFilter
	#' @param fieldNames The field values to give the created DataMiningReportSubtopicStandardFilter. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created DataMiningReportSubtopicStandardFilter
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDataMiningReportSubtopicStandardFilter <- function(IsEnabled = NULL, DataMiningReportID = NULL, SubtopicStandardFilterID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "DataMiningReportSubtopicStandardFilter", body = list(DataObject = body), searchFields = append("DataMiningReportSubtopicStandardFilterID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DataMiningReportSubtopicStandardFilter
	#'
	#' This function modifies a DataMiningReportSubtopicStandardFilter
	#' @param fieldNames The field values to give the modified DataMiningReportSubtopicStandardFilter. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified DataMiningReportSubtopicStandardFilter
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDataMiningReportSubtopicStandardFilter <- function(DataMiningReportSubtopicStandardFilterID, IsEnabled = NULL, DataMiningReportID = NULL, SubtopicStandardFilterID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "DataMiningReportSubtopicStandardFilter", objectId = DataMiningReportSubtopicStandardFilterID, body = list(DataObject = body), searchFields = append("DataMiningReportSubtopicStandardFilterID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SubtopicStandardFilters
	#'
	#' This function returns a dataframe or json object of SubtopicStandardFilters
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SubtopicStandardFilters. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SubtopicStandardFilters.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SubtopicStandardFilter') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of SubtopicStandardFilters
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSubtopicStandardFilters <- function(searchConditionsList = NULL, SubtopicStandardFilterID = F, SkywardID = F, SkywardHash = F, Path = F, SubtopicID = F, StandardFilterID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsRequired = F, DisplayOnReport = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "SubtopicStandardFilter", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SubtopicStandardFilter
	#'
	#' This function returns a dataframe or json object of a SubtopicStandardFilter
	#' @param SubtopicStandardFilterID The ID of the SubtopicStandardFilter to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SubtopicStandardFilter. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SubtopicStandardFilter.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SubtopicStandardFilter') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of SubtopicStandardFilter
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSubtopicStandardFilter <- function(SubtopicStandardFilterID, SkywardID = F, SkywardHash = F, Path = F, SubtopicID = F, StandardFilterID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsRequired = F, DisplayOnReport = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SubtopicStandardFilterID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "SubtopicStandardFilter", objectId = SubtopicStandardFilterID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SubtopicStandardFilter
	#'
	#' This function deletes a SubtopicStandardFilter
	#' @param SubtopicStandardFilterID The ID of the SubtopicStandardFilter to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The SubtopicStandardFilterID of the deleted SubtopicStandardFilter.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSubtopicStandardFilter <- function(SubtopicStandardFilterID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "SubtopicStandardFilter", objectId = SubtopicStandardFilterID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SubtopicStandardFilter
	#'
	#' This function creates a SubtopicStandardFilter
	#' @param fieldNames The field values to give the created SubtopicStandardFilter. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created SubtopicStandardFilter
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSubtopicStandardFilter <- function(Path = NULL, SubtopicID = NULL, StandardFilterID = NULL, IsRequired = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "SubtopicStandardFilter", body = list(DataObject = body), searchFields = append("SubtopicStandardFilterID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SubtopicStandardFilter
	#'
	#' This function modifies a SubtopicStandardFilter
	#' @param fieldNames The field values to give the modified SubtopicStandardFilter. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified SubtopicStandardFilter
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySubtopicStandardFilter <- function(SubtopicStandardFilterID, Path = NULL, SubtopicID = NULL, StandardFilterID = NULL, IsRequired = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "SubtopicStandardFilter", objectId = SubtopicStandardFilterID, body = list(DataObject = body), searchFields = append("SubtopicStandardFilterID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DataMiningReportFieldParameters
	#'
	#' This function returns a dataframe or json object of DataMiningReportFieldParameters
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DataMiningReportFieldParameters. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DataMiningReportFieldParameters.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DataMiningReportFieldParameter') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of DataMiningReportFieldParameters
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDataMiningReportFieldParameters <- function(searchConditionsList = NULL, DataMiningReportFieldParameterID = F, ParameterName = F, DataType = F, Value = F, IsPrompt = F, PromptLabel = F, PromptLabelOrParameterName = F, DataMiningReportID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "DataMiningReportFieldParameter", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DataMiningReportFieldParameter
	#'
	#' This function returns a dataframe or json object of a DataMiningReportFieldParameter
	#' @param DataMiningReportFieldParameterID The ID of the DataMiningReportFieldParameter to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DataMiningReportFieldParameter. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DataMiningReportFieldParameter.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DataMiningReportFieldParameter') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of DataMiningReportFieldParameter
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDataMiningReportFieldParameter <- function(DataMiningReportFieldParameterID, ParameterName = F, DataType = F, Value = F, IsPrompt = F, PromptLabel = F, PromptLabelOrParameterName = F, DataMiningReportID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DataMiningReportFieldParameterID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "DataMiningReportFieldParameter", objectId = DataMiningReportFieldParameterID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DataMiningReportFieldParameter
	#'
	#' This function deletes a DataMiningReportFieldParameter
	#' @param DataMiningReportFieldParameterID The ID of the DataMiningReportFieldParameter to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The DataMiningReportFieldParameterID of the deleted DataMiningReportFieldParameter.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDataMiningReportFieldParameter <- function(DataMiningReportFieldParameterID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "DataMiningReportFieldParameter", objectId = DataMiningReportFieldParameterID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DataMiningReportFieldParameter
	#'
	#' This function creates a DataMiningReportFieldParameter
	#' @param fieldNames The field values to give the created DataMiningReportFieldParameter. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created DataMiningReportFieldParameter
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDataMiningReportFieldParameter <- function(ParameterName = NULL, IsPrompt = NULL, PromptLabel = NULL, DataMiningReportID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "DataMiningReportFieldParameter", body = list(DataObject = body), searchFields = append("DataMiningReportFieldParameterID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DataMiningReportFieldParameter
	#'
	#' This function modifies a DataMiningReportFieldParameter
	#' @param fieldNames The field values to give the modified DataMiningReportFieldParameter. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified DataMiningReportFieldParameter
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDataMiningReportFieldParameter <- function(DataMiningReportFieldParameterID, ParameterName = NULL, IsPrompt = NULL, PromptLabel = NULL, DataMiningReportID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "DataMiningReportFieldParameter", objectId = DataMiningReportFieldParameterID, body = list(DataObject = body), searchFields = append("DataMiningReportFieldParameterID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempUploadDataMiningReportLogs
	#'
	#' This function returns a dataframe or json object of TempUploadDataMiningReportLogs
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempUploadDataMiningReportLogs. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempUploadDataMiningReportLogs.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempUploadDataMiningReportLog') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of TempUploadDataMiningReportLogs
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempUploadDataMiningReportLogs <- function(searchConditionsList = NULL, TempUploadDataMiningReportLogID = F, FileName = F, Message = F, ResultCode = F, LogID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "TempUploadDataMiningReportLog", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempUploadDataMiningReportLog
	#'
	#' This function returns a dataframe or json object of a TempUploadDataMiningReportLog
	#' @param TempUploadDataMiningReportLogID The ID of the TempUploadDataMiningReportLog to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempUploadDataMiningReportLog. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempUploadDataMiningReportLog.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempUploadDataMiningReportLog') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of TempUploadDataMiningReportLog
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempUploadDataMiningReportLog <- function(TempUploadDataMiningReportLogID, FileName = F, Message = F, ResultCode = F, LogID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempUploadDataMiningReportLogID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "TempUploadDataMiningReportLog", objectId = TempUploadDataMiningReportLogID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempUploadDataMiningReportLog
	#'
	#' This function deletes a TempUploadDataMiningReportLog
	#' @param TempUploadDataMiningReportLogID The ID of the TempUploadDataMiningReportLog to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The TempUploadDataMiningReportLogID of the deleted TempUploadDataMiningReportLog.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempUploadDataMiningReportLog <- function(TempUploadDataMiningReportLogID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "TempUploadDataMiningReportLog", objectId = TempUploadDataMiningReportLogID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempUploadDataMiningReportLog
	#'
	#' This function creates a TempUploadDataMiningReportLog
	#' @param fieldNames The field values to give the created TempUploadDataMiningReportLog. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created TempUploadDataMiningReportLog
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempUploadDataMiningReportLog <- function(FileName = NULL, Message = NULL, ResultCode = NULL, LogID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "TempUploadDataMiningReportLog", body = list(DataObject = body), searchFields = append("TempUploadDataMiningReportLogID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempUploadDataMiningReportLog
	#'
	#' This function modifies a TempUploadDataMiningReportLog
	#' @param fieldNames The field values to give the modified TempUploadDataMiningReportLog. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified TempUploadDataMiningReportLog
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempUploadDataMiningReportLog <- function(TempUploadDataMiningReportLogID, FileName = NULL, Message = NULL, ResultCode = NULL, LogID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "TempUploadDataMiningReportLog", objectId = TempUploadDataMiningReportLogID, body = list(DataObject = body), searchFields = append("TempUploadDataMiningReportLogID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ReportErrors
	#'
	#' This function returns a dataframe or json object of ReportErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReportErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReportErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReportError') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of ReportErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listReportErrors <- function(searchConditionsList = NULL, ReportErrorID = F, ReportID = F, ErrorMessage = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "ReportError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ReportError
	#'
	#' This function returns a dataframe or json object of a ReportError
	#' @param ReportErrorID The ID of the ReportError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReportError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReportError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReportError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of ReportError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getReportError <- function(ReportErrorID, ReportID = F, ErrorMessage = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ReportErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "ReportError", objectId = ReportErrorID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ReportError
	#'
	#' This function deletes a ReportError
	#' @param ReportErrorID The ID of the ReportError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The ReportErrorID of the deleted ReportError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteReportError <- function(ReportErrorID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "ReportError", objectId = ReportErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ReportError
	#'
	#' This function creates a ReportError
	#' @param fieldNames The field values to give the created ReportError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created ReportError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createReportError <- function(ReportID = NULL, ErrorMessage = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "ReportError", body = list(DataObject = body), searchFields = append("ReportErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ReportError
	#'
	#' This function modifies a ReportError
	#' @param fieldNames The field values to give the modified ReportError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified ReportError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyReportError <- function(ReportErrorID, ReportID = NULL, ErrorMessage = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "ReportError", objectId = ReportErrorID, body = list(DataObject = body), searchFields = append("ReportErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempReportRoles
	#'
	#' This function returns a dataframe or json object of TempReportRoles
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempReportRoles. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempReportRoles.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempReportRole') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of TempReportRoles
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempReportRoles <- function(searchConditionsList = NULL, TempReportRoleID = F, ReportRoleID = F, ReportID = F, ReportName = F, ReportPortalCode = F, ReportPrimaryMenuModuleDisplayName = F, ReportReportTypeCode = F, ReportIsSkywardMaintained = F, RoleID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ReportIsWSIPCMaintained = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "TempReportRole", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempReportRole
	#'
	#' This function returns a dataframe or json object of a TempReportRole
	#' @param TempReportRoleID The ID of the TempReportRole to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempReportRole. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempReportRole.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempReportRole') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of TempReportRole
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempReportRole <- function(TempReportRoleID, ReportRoleID = F, ReportID = F, ReportName = F, ReportPortalCode = F, ReportPrimaryMenuModuleDisplayName = F, ReportReportTypeCode = F, ReportIsSkywardMaintained = F, RoleID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ReportIsWSIPCMaintained = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempReportRoleID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "TempReportRole", objectId = TempReportRoleID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempReportRole
	#'
	#' This function deletes a TempReportRole
	#' @param TempReportRoleID The ID of the TempReportRole to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The TempReportRoleID of the deleted TempReportRole.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempReportRole <- function(TempReportRoleID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "TempReportRole", objectId = TempReportRoleID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempReportRole
	#'
	#' This function creates a TempReportRole
	#' @param fieldNames The field values to give the created TempReportRole. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created TempReportRole
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempReportRole <- function(ReportRoleID = NULL, ReportID = NULL, ReportName = NULL, ReportPortalCode = NULL, ReportPrimaryMenuModuleDisplayName = NULL, ReportReportTypeCode = NULL, ReportIsSkywardMaintained = NULL, RoleID = NULL, ReportIsWSIPCMaintained = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "TempReportRole", body = list(DataObject = body), searchFields = append("TempReportRoleID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempReportRole
	#'
	#' This function modifies a TempReportRole
	#' @param fieldNames The field values to give the modified TempReportRole. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified TempReportRole
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempReportRole <- function(TempReportRoleID, ReportRoleID = NULL, ReportID = NULL, ReportName = NULL, ReportPortalCode = NULL, ReportPrimaryMenuModuleDisplayName = NULL, ReportReportTypeCode = NULL, ReportIsSkywardMaintained = NULL, RoleID = NULL, ReportIsWSIPCMaintained = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "TempReportRole", objectId = TempReportRoleID, body = list(DataObject = body), searchFields = append("TempReportRoleID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ReportQueueDestinationResults
	#'
	#' This function returns a dataframe or json object of ReportQueueDestinationResults
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReportQueueDestinationResults. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReportQueueDestinationResults.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReportQueueDestinationResult') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of ReportQueueDestinationResults
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listReportQueueDestinationResults <- function(searchConditionsList = NULL, ReportQueueDestinationResultID = F, ReportQueueID = F, FileDestinationResultID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "ReportQueueDestinationResult", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ReportQueueDestinationResult
	#'
	#' This function returns a dataframe or json object of a ReportQueueDestinationResult
	#' @param ReportQueueDestinationResultID The ID of the ReportQueueDestinationResult to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReportQueueDestinationResult. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReportQueueDestinationResult.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReportQueueDestinationResult') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of ReportQueueDestinationResult
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getReportQueueDestinationResult <- function(ReportQueueDestinationResultID, ReportQueueID = F, FileDestinationResultID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ReportQueueDestinationResultID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "ReportQueueDestinationResult", objectId = ReportQueueDestinationResultID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ReportQueueDestinationResult
	#'
	#' This function deletes a ReportQueueDestinationResult
	#' @param ReportQueueDestinationResultID The ID of the ReportQueueDestinationResult to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The ReportQueueDestinationResultID of the deleted ReportQueueDestinationResult.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteReportQueueDestinationResult <- function(ReportQueueDestinationResultID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "ReportQueueDestinationResult", objectId = ReportQueueDestinationResultID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ReportQueueDestinationResult
	#'
	#' This function creates a ReportQueueDestinationResult
	#' @param fieldNames The field values to give the created ReportQueueDestinationResult. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created ReportQueueDestinationResult
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createReportQueueDestinationResult <- function(ReportQueueID = NULL, FileDestinationResultID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "ReportQueueDestinationResult", body = list(DataObject = body), searchFields = append("ReportQueueDestinationResultID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ReportQueueDestinationResult
	#'
	#' This function modifies a ReportQueueDestinationResult
	#' @param fieldNames The field values to give the modified ReportQueueDestinationResult. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified ReportQueueDestinationResult
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyReportQueueDestinationResult <- function(ReportQueueDestinationResultID, ReportQueueID = NULL, FileDestinationResultID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "ReportQueueDestinationResult", objectId = ReportQueueDestinationResultID, body = list(DataObject = body), searchFields = append("ReportQueueDestinationResultID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ScheduledReportDestinations
	#'
	#' This function returns a dataframe or json object of ScheduledReportDestinations
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ScheduledReportDestinations. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ScheduledReportDestinations.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ScheduledReportDestination') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A list of ScheduledReportDestinations
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listScheduledReportDestinations <- function(searchConditionsList = NULL, ScheduledReportDestinationID = F, ScheduledReportID = F, FileDestinationID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Reporting", objectName = "ScheduledReportDestination", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ScheduledReportDestination
	#'
	#' This function returns a dataframe or json object of a ScheduledReportDestination
	#' @param ScheduledReportDestinationID The ID of the ScheduledReportDestination to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ScheduledReportDestination. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ScheduledReportDestination.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ScheduledReportDestination') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A dataframe or of ScheduledReportDestination
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getScheduledReportDestination <- function(ScheduledReportDestinationID, ScheduledReportID = F, FileDestinationID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ScheduledReportDestinationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Reporting", objectName = "ScheduledReportDestination", objectId = ScheduledReportDestinationID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ScheduledReportDestination
	#'
	#' This function deletes a ScheduledReportDestination
	#' @param ScheduledReportDestinationID The ID of the ScheduledReportDestination to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The ScheduledReportDestinationID of the deleted ScheduledReportDestination.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteScheduledReportDestination <- function(ScheduledReportDestinationID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Reporting", objectName = "ScheduledReportDestination", objectId = ScheduledReportDestinationID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ScheduledReportDestination
	#'
	#' This function creates a ScheduledReportDestination
	#' @param fieldNames The field values to give the created ScheduledReportDestination. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return A newly created ScheduledReportDestination
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createScheduledReportDestination <- function(ScheduledReportID = NULL, FileDestinationID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Reporting", objectName = "ScheduledReportDestination", body = list(DataObject = body), searchFields = append("ScheduledReportDestinationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ScheduledReportDestination
	#'
	#' This function modifies a ScheduledReportDestination
	#' @param fieldNames The field values to give the modified ScheduledReportDestination. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Reporting
	#' @return The modified ScheduledReportDestination
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyScheduledReportDestination <- function(ScheduledReportDestinationID, ScheduledReportID = NULL, FileDestinationID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Reporting", objectName = "ScheduledReportDestination", objectId = ScheduledReportDestinationID, body = list(DataObject = body), searchFields = append("ScheduledReportDestinationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}
