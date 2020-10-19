
	#' List SupportCenterAttachments
	#'
	#' This function returns a dataframe or json object of SupportCenterAttachments
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SupportCenterAttachment') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @concept Support Center
	#' @return A list of SupportCenterAttachments
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSupportCenterAttachments <- function(searchConditionsList = NULL, AttachmentID = F, ProjectID = F, ActionID = F, RMSAttachmentID = F, Name = F, MediaID = F, Created = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SupportCenter", objectName = "Attachment", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten)
	}

	#' List SupportCenterActions
	#'
	#' This function returns a dataframe or json object of SupportCenterActions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SupportCenterAction') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @concept Support Center
	#' @return A list of SupportCenterActions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSupportCenterActions <- function(searchConditionsList = NULL, ActionID = F, ProjectID = F, RMSActionID = F, RMSEmailID = F, Type = F, EmailTo = F, EmailFrom = F, Summary = F, Description = F, StartTime = F, StopTime = F, Duration = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SupportCenter", objectName = "Action", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten)
	}

	#' List SupportCenterProjects
	#'
	#' This function returns a dataframe or json object of SupportCenterProjects
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('SupportCenterProject') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @concept Support Center
	#' @return A list of SupportCenterProjects
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSupportCenterProjects <- function(searchConditionsList = NULL, ProjectID = F, RMSProjectID = F, Type = F, Summary = F, Description = F, Status = F, UserID = F, SecurityLocation = F, ReportedRelease = F, Critical = F, Origin = F, Created = F, Modified = F, Resolution = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TypeDetails = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SupportCenter", objectName = "Project", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten)
	}

	#' List NewFeatureModulePaths
	#'
	#' This function returns a dataframe or json object of NewFeatureModulePaths
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('NewFeatureModulePath') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @concept Support Center
	#' @return A list of NewFeatureModulePaths
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNewFeatureModulePaths <- function(searchConditionsList = NULL, NewFeatureModulePathID = F, NewFeatureID = F, Module = F, Object = F, Name = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SupportCenter", objectName = "NewFeatureModulePath", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten)
	}

	#' List NewFeatureUsers
	#'
	#' This function returns a dataframe or json object of NewFeatureUsers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('NewFeatureUser') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @concept Support Center
	#' @return A list of NewFeatureUsers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNewFeatureUsers <- function(searchConditionsList = NULL, NewFeatureUserID = F, NewFeatureID = F, UserIDViewer = F, ViewCount = F, MarkedAsRead = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SupportCenter", objectName = "NewFeatureUser", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten)
	}

	#' List NewFeatures
	#'
	#' This function returns a dataframe or json object of NewFeatures
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given object. Defaults to FALSE for all return fields which, for convenience, returns all fields for the object.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object set to TRUE or FALSE. Run getSchemaForObject('NewFeature') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{getAllSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{getAllEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{getAllSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @concept Support Center
	#' @return A list of NewFeatures
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNewFeatures <- function(searchConditionsList = NULL, NewFeatureID = F, RMSID = F, ReleasedVersionMajor = F, ReleasedVersionMinor = F, ReleasedVersionBuild = F, ReleasedVersionRevision = F, Duration = F, PRNumber = F, Summary = F, Tagline = F, Description = F, Type = F, Portal = F, VideoURL = F, DocumentationURL = F, PDCURL = F, ReleaseNotesURL = F, SystemVersionID = F, IsActive = F, IsNew = F, Modules = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SupportCenter", objectName = "NewFeature", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten)
	}
